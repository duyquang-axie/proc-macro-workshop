use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, GenericArgument, PathArguments, Type};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let struct_name = &input.ident;
    let builder_name = quote::format_ident!("{}Builder", struct_name);
    let mut non_opt_ident_fields = Vec::new();
    let mut non_opt_ty_fields = Vec::new();
    let mut opt_ident_fields = Vec::new();
    let mut opt_ty_fields = Vec::new();

    if let Data::Struct(struct_data) = &input.data {
        for field in &struct_data.fields {
            if let Some(ident) = &field.ident {
                // ident_fields.push(ident);
                let mut is_opt = false;
                let mut ty = &field.ty;
                if let Type::Path(path) = &field.ty {
                    if path.qself.is_some() {
                        continue;
                    }
                    if let Some(first_segment) = path.path.segments.first() {
                        if first_segment.ident == "Option" {
                            is_opt = true;
                            if let PathArguments::AngleBracketed(args) = &first_segment.arguments {
                                if let GenericArgument::Type(inner_opt_ty) =
                                    args.args.first().unwrap()
                                {
                                    ty = inner_opt_ty;
                                }
                            }
                        }
                    }
                }

                if is_opt {
                    opt_ty_fields.push(ty);
                    opt_ident_fields.push(ident);
                } else {
                    non_opt_ty_fields.push(ty);
                    non_opt_ident_fields.push(ident);
                }
            }
        }
    }

    let ident_fields = non_opt_ident_fields
        .iter()
        .chain(opt_ident_fields.iter())
        .collect::<Vec<_>>();
    let ty_fields = non_opt_ty_fields
        .iter()
        .chain(opt_ty_fields.iter())
        .collect::<Vec<_>>();

    let expanded = quote! {
        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#ident_fields: None),*
                }
            }
        }

        pub struct #builder_name {
            #(#ident_fields: Option<#ty_fields>),*
        }

        impl #builder_name {
            #(fn #ident_fields(&mut self, #ident_fields: #ty_fields) -> &mut Self {
                self.#ident_fields = Some(#ident_fields);
                self
            })*


            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                // check all field exist
                #(if self.#non_opt_ident_fields.is_none() {
                    Err(format!("field {} is empty", stringify!(#non_opt_ident_fields)))?;
                })*


                Ok(#struct_name {
                    #(#non_opt_ident_fields: self.#non_opt_ident_fields.take().unwrap() ,)*
                    #(#opt_ident_fields: self.#opt_ident_fields.take()),*
                })
            }
        }
    };

    TokenStream::from(expanded)
}
