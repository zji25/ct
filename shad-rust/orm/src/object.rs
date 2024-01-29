use crate::{
    data::DataType,
    storage::{Row, RowSlice},
};

use std::any::Any;

////////////////////////////////////////////////////////////////////////////////

pub trait Object: Any {
    const SCHEMA: Schema;
    fn to_row(&self) -> Row;
    fn from_row(row: &RowSlice) -> Self;
    fn schema(&self) -> &'static Schema {
        &Self::SCHEMA
    }
}

pub trait Store {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
    fn get_schema(&self) -> &'static Schema;
    fn get_row(&self) -> Row;
}

impl<T: Object> Store for T {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
    fn get_schema(&self) -> &'static Schema {
        self.schema()
    }
    fn get_row(&self) -> Row {
        self.to_row()
    }
}

////////////////////////////////////////////////////////////////////////////////

pub struct SchemaField {
    pub field_name: &'static str,
    pub column_name: &'static str,
    pub data_type: DataType,
}

impl SchemaField {
    pub fn to_sql(&self) -> String {
        format!(", {} {}", self.column_name, self.data_type.to_sql())
    }
}

pub struct Schema {
    pub type_name: &'static str,
    pub table_name: &'static str,
    pub fields: &'static [SchemaField],
}

impl Schema {
    pub fn get_column_names(&self) -> String {
        self.fields
            .iter()
            .map(|sf| sf.column_name)
            .collect::<Vec<_>>()
            .join(", ")
    }
    pub fn get_field(&self, column: &str) -> &'static SchemaField {
        self.fields
            .iter()
            .find(|&sf| sf.column_name == column)
            .unwrap()
    }
}
