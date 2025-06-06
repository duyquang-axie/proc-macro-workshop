use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Type};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = &input.ident;
    let builder_name = quote::format_ident!("{}Builder", struct_name);

    if let Data::Struct(struct_data) = &input.data {
        for field in &struct_data.fields {
            if let Some(ident) = &field.ident {
                // println!("ident={}", ident);
                match &field.ty {
                    Type::Path(path) => {
                        println!(
                            "ident={}, path={:?}, path_len={}",
                            ident,
                            path.path.segments[0].ident,
                            // path.path.get_ident(),
                            path.path.segments.len(),
                        );
                    }
                    _ => {}
                }
            }
        }
    }

    let expanded = quote! {
        impl #struct_name {
            pub fn builder() {}
        }

        pub struct #builder_name {
            executable: Option<String>,
            args: Option<Vec<String>>,
            env: Option<Vec<String>>,
            current_dir: Option<String>,
        }
    };

    TokenStream::from(expanded)
}
