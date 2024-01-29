use crate::{
    data::{DataType, Value},
    error::{Error, MissingColumnError, NotFoundError, Result, UnexpectedTypeError},
    object::{Schema, SchemaField},
    ObjectId,
};

use rusqlite::{params, ToSql};

use std::borrow::Cow;

////////////////////////////////////////////////////////////////////////////////

pub type Row<'a> = Vec<Value<'a>>;
pub type RowSlice<'a> = [Value<'a>];

////////////////////////////////////////////////////////////////////////////////

pub(crate) trait StorageTransaction {
    fn table_exists(&self, table: &str) -> Result<bool>;
    fn create_table(&self, schema: &Schema) -> Result<()>;
    fn insert_row(&self, schema: &Schema, row: &RowSlice) -> Result<ObjectId>;
    fn update_row(&self, id: ObjectId, schema: &Schema, row: &RowSlice) -> Result<()>;
    fn select_row(&self, id: ObjectId, schema: &Schema) -> Result<Row<'static>>;
    fn delete_row(&self, id: ObjectId, schema: &Schema) -> Result<()>;
    fn commit(&self) -> Result<()>;
    fn rollback(&self) -> Result<()>;
}

impl<'a> StorageTransaction for rusqlite::Transaction<'a> {
    fn table_exists(&self, table: &str) -> Result<bool> {
        let count: i64 = self.query_row(
            "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = ?",
            [table],
            |row| row.get(0),
        )?;
        Ok(count > 0)
    }

    fn create_table(&self, schema: &Schema) -> Result<()> {
        self.execute(
            &format!(
                "CREATE TABLE {} ( id INTEGER PRIMARY KEY AUTOINCREMENT{})",
                schema.table_name,
                schema
                    .fields
                    .iter()
                    .map(SchemaField::to_sql)
                    .collect::<Vec<String>>()
                    .join("")
            ),
            [],
        )?;
        Ok(())
    }

    fn insert_row(&self, schema: &Schema, row: &RowSlice) -> Result<ObjectId> {
        match if schema.fields.is_empty() {
            self.execute(
                &format!("INSERT INTO {} DEFAULT VALUES", schema.table_name),
                [],
            )
        } else {
            self.execute(
                &format!(
                    "INSERT INTO {}({}) VALUES ({})",
                    schema.table_name,
                    schema.get_column_names(),
                    (1..=schema.fields.len())
                        .map(|i| format!("?{}", i))
                        .collect::<Vec<String>>()
                        .join(", "),
                ),
                &*row
                    .iter()
                    .map(|x| x as &dyn ToSql)
                    .collect::<Vec<&dyn ToSql>>(),
            )
        } {
            Ok(_) => Ok(self.last_insert_rowid().into()),
            Err(error) => {
                let reg = format!("table {} has no column named ", schema.table_name);
                if error.to_string().starts_with(&reg) {
                    let column = error.to_string().replace(&reg, "");
                    let c = schema.get_field(&column);
                    return Err(Error::MissingColumn(Box::new(MissingColumnError {
                        type_name: schema.type_name,
                        attr_name: c.field_name,
                        table_name: schema.table_name,
                        column_name: c.column_name,
                    })));
                }
                Err(error.into())
            }
        }
    }

    fn update_row(&self, id: ObjectId, schema: &Schema, row: &RowSlice) -> Result<()> {
        self.execute(
            &format!(
                "UPDATE {} SET {} WHERE id = {}",
                schema.table_name,
                schema
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, sf)| format!("{} = ?{}", sf.column_name, i + 1))
                    .collect::<Vec<_>>()
                    .join(", "),
                Into::<i64>::into(id)
            ),
            &*row
                .iter()
                .map(|x| x as &dyn ToSql)
                .collect::<Vec<&dyn ToSql>>(),
        )?;
        Ok(())
    }

    fn select_row(&self, id: ObjectId, schema: &Schema) -> Result<Row<'static>> {
        println!("sssss {}", schema.fields.len());
        match self.prepare(&format!(
            "SELECT {} FROM {} WHERE id = (?1)",
            if schema.fields.is_empty() {
                "*".to_string()
            } else {
                schema.get_column_names()
            },
            schema.table_name
        )) {
            Ok(mut q) => {
                let row = q.query_row(params![Into::<i64>::into(id)], |row| {
                    schema
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, sf)| {
                            Ok(match sf.data_type {
                                DataType::String => Value::String(Cow::Owned(row.get(i)?)),
                                DataType::Bytes => Value::Bytes(Cow::Owned(row.get(i)?)),
                                DataType::Int64 => Value::Int64(row.get(i)?),
                                DataType::Float64 => Value::Float64(row.get(i)?),
                                DataType::Bool => Value::Bool(row.get(i)?),
                            })
                        })
                        .collect::<std::result::Result<Vec<Value>, rusqlite::Error>>()
                });
                match row {
                    Ok(roww) => Ok(roww),
                    Err(rusqlite::Error::QueryReturnedNoRows) => {
                        Err(Error::NotFound(Box::new(NotFoundError {
                            object_id: id,
                            type_name: schema.type_name,
                        })))
                    }
                    Err(rusqlite::Error::InvalidColumnType(i, s, t)) => {
                        println!("{} {} {}", i, s, t);
                        let sf = schema.get_field(&s);
                        println!("{}", sf.column_name);
                        Err(Error::UnexpectedType(Box::new(UnexpectedTypeError {
                            type_name: schema.type_name,
                            attr_name: sf.field_name,
                            table_name: schema.table_name,
                            column_name: sf.column_name,
                            expected_type: sf.data_type,
                            got_type: t.to_string(),
                        })))
                    }
                    Err(err) => Err(err.into()),
                }
            }

            Err(e) => {
                if e.to_string().starts_with("no such column: ") {
                    let column = e.to_string().replace("no such column: ", "");
                    let c = schema.get_field(&column);
                    return Err(Error::MissingColumn(Box::new(MissingColumnError {
                        type_name: schema.type_name,
                        attr_name: c.field_name,
                        table_name: schema.table_name,
                        column_name: c.column_name,
                    })));
                }
                Err(e.into())
            }
        }
    }

    fn delete_row(&self, id: ObjectId, schema: &Schema) -> Result<()> {
        self.execute(
            &format!("DELETE FROM {} WHERE id = ?", schema.table_name),
            params![Into::<i64>::into(id)],
        )?;
        Ok(())
    }

    fn commit(&self) -> Result<()> {
        self.execute("COMMIT", [])?;
        Ok(())
    }

    fn rollback(&self) -> Result<()> {
        self.execute("ROLLBACK", [])?;
        Ok(())
    }
}
