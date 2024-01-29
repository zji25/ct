use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Data::Struct, Fields};

#[proc_macro_derive(Scan)]
pub fn derive_scan(stream: TokenStream) -> TokenStream {
    let DeriveInput {
        ident: struct_ident,
        data,
        generics,
        ..
      }: DeriveInput = parse_macro_input!(stream);
    let Struct(strct) = data else { panic!("only structs are supported") };
    let fields_impl = match strct.fields {
        Fields::Named(named_fields) => {
            named_fields.named.into_iter().map(|field| {
                let field_name = field.ident;
                quote! {
                    gcs.extend((&self.#field_name as &dyn Scan).get_gcs());
                }
            }).collect()
        }
        Fields::Unnamed(_) => todo!(),
        Fields::Unit => vec![],
    };
    let (impl_gen, ty_gen, where_clause) = generics.split_for_impl(); 
    // let struct_ident = struct_name_ident;
    let output = quote![
        impl #impl_gen Scan for #struct_ident #ty_gen
        #where_clause
        {
            fn get_gcs(&self) -> Vec<usize> {
                let mut gcs = Vec::new();
                #(#fields_impl)*
                gcs
            } 
        }
    ];
    output.into()
}
