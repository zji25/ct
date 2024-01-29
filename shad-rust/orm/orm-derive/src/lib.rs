use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, 
	Attribute, 
	Data::Struct, 
	DataStruct,
    DeriveInput, 
	Fields::Named, 
	FieldsNamed, 
	Ident, Lit::Str, 
	Meta::List, 
	MetaList,
    NestedMeta::Lit,
};


#[proc_macro_derive(Object, attributes(table_name, column_name))]
pub fn derive_object(stream: TokenStream) -> TokenStream {
    let DeriveInput {
        ident: struct_ident,
        data,
        generics,
        attrs,
        ..
    }: DeriveInput = parse_macro_input!(stream);

    let (impl_gen, ty_gen, where_clause) = generics.split_for_impl();
    let type_name = struct_ident.to_string();
    let table_name = get_attribute("table_name", &attrs, &type_name);

    let Struct(DataStruct { fields, .. }) = data else {
        return quote![compile_error!("#[derive(Object)] only supports structs");].into();
    };
    let mut field_idents: Vec<Ident> = Vec::new();
    let schema_fields = 
    if let Named(FieldsNamed { named, .. }) = fields {
        named.into_iter().map(
            |field| {
                let field_ident = field.ident.unwrap();
                let field_name = field_ident.to_string();
                let column_name = get_attribute("column_name", &field.attrs, &field_name);
                let t: syn::Type = field.ty;
                let syn::Type::Path(type_path) = t else {todo!()};
                let field_datatype = get_type(&quote!(#type_path).to_string());
                field_idents.push(field_ident);
                
                quote![
                    orm::object::SchemaField {
                        field_name: #field_name,
                        column_name: #column_name,
                        data_type: orm::data::DataType::#field_datatype,
                    }
                ]
            }
        ).collect()
    } 
    else {
        vec!()
    };
   

    quote![
		impl #impl_gen Object for #struct_ident #ty_gen
		#where_clause
		{
			const SCHEMA: orm::object::Schema = orm::object::Schema {
				type_name: #type_name,
				table_name: #table_name,
				fields: &[#(#schema_fields),*],
			};
			fn to_row(&self) -> orm::storage::Row {
				vec![#(orm::data::Value::from(&self.#field_idents)),*]
			}
			fn from_row(row: &orm::storage::RowSlice) -> Self {
				let mut row_iter = row.iter();
				Self {
					#(#field_idents: row_iter.next().unwrap().into(),)*
				}
			}
		}
    ].into()
}


fn get_attribute(ident: &str, attrs: &[Attribute], default: &str) -> String {
    let mut idents = attrs.iter().filter_map(|attr| {
        if let Ok(List(MetaList { path, nested, .. })) = attr.parse_meta() {
            if path.is_ident(ident) {
                let Lit(Str(name)) = nested.first().unwrap() else {
                    todo!()
                };
                return Some(name.value());
            }
        }
        None
    });
    match idents.next() {
        Some(first) => {
            if idents.next().is_some() {
                panic!("multiple {} calls", ident);
            }
            first
        }
        None => default.to_owned(),
    }
}

fn get_type(from: &str) -> Ident {
    syn::parse_str::<Ident>(
        match from {
            "String" => "String",
            "Vec < u8 >" => "Bytes",
            "i64" => "Int64",
            "f64" => "Float64",
            "bool" => "Bool",
            _ => panic!()
        }
    ).unwrap()
}