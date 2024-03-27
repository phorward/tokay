/*! Tokay proc-macros

This crate contains the proc-macro implementation for

- tokay_function!(signature, expression) - Built-in function
- tokay_method!(signature, expression) - Built-in object method
- tokay_token!(signature, expression) - Built-in consuming function

Every macro generates a slightly different version of a callable built-in.

All macros require for two parameters:

- *signature* is a Tokay-style function signature string, including default values.
  This can be `f`, `f()`, `f(a, b)`, `f(a b = void)` or similar.
  Currently, this does only accept for a subset of Tokay atomics: void, null, true, false,
  and integer values.
- *expression* is the Rust expression to be executed. This is the body of the function.
*/

use glob::glob;
use proc_macro::TokenStream;
use proc_macro2;
use quote::{quote, quote_spanned};
use syn;
use tokay_04 as tokay; // This is Tokay v0.4

/* Tokay v0.4 compat, the function has been reworked in v0.5 */
fn tokay_run(src: &str, input: &str) -> Result<Option<tokay::value::Value>, String> {
    // disable any debug inside of this process
    std::env::set_var("TOKAY_DEBUG", "0");
    std::env::set_var("TOKAY_PARSER_DEBUG", "0");

    let mut compiler = tokay::compiler::Compiler::new();
    let program = compiler.compile(tokay::reader::Reader::new(Box::new(std::io::Cursor::new(
        src.to_owned(),
    ))));

    match program {
        Ok(program) => program
            .run_from_string(input.to_owned())
            .map_err(|err| err.to_string()),
        Err(errors) => Err(errors
            .into_iter()
            .map(|err| err.to_string())
            .collect::<Vec<String>>()
            .join("\n")),
    }
}

/// Describes a builtin function and its arguments.
struct BuiltinDef {
    name: syn::Ident,
    arguments: Vec<(String, String)>,
    body: syn::Expr,
}

impl syn::parse::Parse for BuiltinDef {
    fn parse(stream: syn::parse::ParseStream) -> syn::Result<Self> {
        let signature = stream.parse::<syn::LitStr>()?;
        let _ = stream.parse::<syn::Token![,]>()?;
        let body = stream.parse::<syn::Expr>()?;

        // Collect arguments and possible required marker.
        let res = match tokay_run(include_str!("signature.tok"), &signature.value()) {
            Err(msg) => return Err(syn::parse::Error::new(signature.span(), msg)),
            Ok(ast) => ast.unwrap().to_list(),
        };

        let mut arguments = Vec::new();
        let name = res[0].borrow().to_string();

        if res.len() > 1 {
            let args = res[1].borrow().to_list();

            // fixme: This is a little bit ugly but is needed to use Tokay v0.4 here.
            //        It has to be improved when using a higher Tokay version for building later.
            for item in args.iter() {
                let arg = &*item.borrow();
                if let Some(arg) = arg.get_list() {
                    //println!("{} {:?}", name, item);
                    arguments.push((arg[0].borrow().to_string(), arg[1].borrow().to_string()));
                } else {
                    //println!("{} {:?}", name, args);
                    arguments.push((args[0].borrow().to_string(), args[1].borrow().to_string()));
                    break; // Tokay v0.4 special case... don't ask for this.
                }
            }
        }

        Ok(BuiltinDef {
            name: syn::Ident::new(&name, proc_macro2::Span::call_site()),
            arguments,
            body,
        })
    }
}

fn gen_assign_arguments(arguments: Vec<(String, String)>) -> Vec<proc_macro2::TokenStream> {
    let mut ret = Vec::new();

    let mut count: usize = 0;
    let mut args = false;
    let mut nargs = false;

    for (arg, default) in arguments {
        if arg == "*args" {
            // fixme: This must be handled by signature.tok later...
            if args {
                ret.push(quote! {
                    compile_error!("Multiple usage of *args");
                });
            }

            args = true;
            continue;
        } else if arg == "**nargs" {
            // fixme: This must be handled by signature.tok later...
            if nargs {
                ret.push(quote! {
                    compile_error!("Multiple usage of *nargs");
                });
            }

            nargs = true;
            continue;
        }

        count += 1;

        let arg = syn::Ident::new(
            &arg,
            proc_macro2::Span::call_site(), // todo: this can be specified more specific
        );

        ret.push({
            let required = default.is_empty();
            let default = match &default[..] {
                "void" | "" => quote!(tokay::value!(void)),
                "null" => quote!(tokay::value!(null)),
                "true" => quote!(tokay::value!(true)),
                "false" => quote!(tokay::value!(false)),
                int if int.parse::<i64>().is_ok() => {
                    let int = int.parse::<i64>().unwrap();
                    quote!(tokay::value!(#int))
                }
                _ => unreachable!(),
            };

            quote! {
                let mut #arg =
                    if !args.is_empty() {
                        args.remove(0)
                    }
                    else {
                        let mut value = None;

                        if let Some(nargs) = &mut nargs {
                            value = nargs.remove_str(stringify!(#arg));
                        }

                        if value.is_none() {
                            if #required {
                                return Err(format!("{} expected argument '{}'", __function, stringify!(#arg)).into()).into();
                            }
                            else {
                                #default
                            }
                        }
                        else {
                            value.unwrap()
                        }
                    }
                ;

                //println!("{} = {}", stringify!(#arg), #arg);
            }
        });
    }

    if !args {
        ret.push(quote! {
            if args.len() > 0 {
                return Err(
                    match #count {
                        0 => format!("{} doesn't accept any arguments ({} given)", __function, args.len()),
                        1 => format!("{} takes exactly one argument ({} given)", __function, #count + args.len()),
                        _ => format!("{} expected at most {} arguments ({} given)", __function, #count, #count + args.len()),
                    }.into()
                ).into()
            }
        });
    }

    if !nargs {
        ret.push(quote! {
            if let Some(mut nargs) = nargs {
                if let Some((name, _)) = nargs.pop() {
                    return Err(
                        match nargs.len() {
                            0 => format!("{} doesn't accept named argument '{}'", __function, name.to_string()),
                            n => format!("{} doesn't accept named arguments ({} given)", __function, n + 1),
                        }.into()
                    ).into()
                }
            }
        });
    }

    ret
}

#[proc_macro]
pub fn tokay_method(input: TokenStream) -> TokenStream {
    let def = syn::parse_macro_input!(input as BuiltinDef);

    let name = def.name;
    let internal = syn::Ident::new(
        &format!("{}_internal", name.to_string()),
        proc_macro2::Span::call_site(),
    );
    let callable = syn::Ident::new(
        &format!("tokay_method_{}", name.to_string()),
        proc_macro2::Span::call_site(),
    );

    // Method names must start with a lower-case letter
    if !name.to_string().chars().next().unwrap().is_lowercase() {
        return quote_spanned! {
            name.span() => compile_error!(
                "Method identifier must start with a lower-case letter"
            );
        }
        .into();
    }

    // Generate assignment to identifier for each argument.
    let arguments = gen_assign_arguments(def.arguments);
    let body = def.body;

    // Generate two functions: One for direct usage from other Rust code,
    // and one wrapping function for calls from the Tokay VM or a Method.
    // The direct usage function will return an Result<RefValue, Error>
    // instead of an Result<Accept, Reject>.
    let gen = quote! {
        fn #internal(
            context: Option<&mut tokay::Context>,
            mut args: Vec<tokay::RefValue>,
            mut nargs: Option<tokay::Dict>
        ) -> Result<tokay::RefValue, tokay::Error> {
            // The function's original name in Tokay
            let __function = concat!(stringify!(#name), "()");

            // Arguments
            #(#arguments)*

            // Body
            #body
        }

        pub fn #name(
            args: Vec<tokay::RefValue>,
            nargs: Option<tokay::Dict>
        ) -> Result<tokay::RefValue, tokay::Error> {
            Self::#internal(None, args, nargs)
        }

        pub fn #callable(
            context: Option<&mut tokay::Context>,
            args: Vec<tokay::RefValue>,
            nargs: Option<tokay::Dict>
        ) -> Result<tokay::Accept, tokay::Reject> {
            let ret = Self::#internal(context, args, nargs)?;
            Ok(tokay::Accept::Push(tokay::Capture::Value(ret, None, 10)))
        }
    };

    //println!("{} {:?}", function.to_string(), def.required);

    TokenStream::from(gen)
}

#[proc_macro]
pub fn tokay_function(input: TokenStream) -> TokenStream {
    let def = syn::parse_macro_input!(input as BuiltinDef);

    let name = def.name;
    let callable = syn::Ident::new(
        &format!("tokay_function_{}", name.to_string()),
        proc_macro2::Span::call_site(),
    );

    // Function names must start with a lower-case letter
    if !name.to_string().chars().next().unwrap().is_lowercase() {
        return quote_spanned! {
            name.span() => compile_error!(
                "Function identifier must start with a lower-case letter"
            );
        }
        .into();
    }

    // Generate assignment to identifier for each argument.
    let arguments = gen_assign_arguments(def.arguments);
    let body = def.body;

    // Generate function
    let gen = quote! {
        pub fn #callable(
            context: Option<&mut tokay::vm::Context>,
            mut args: Vec<tokay::RefValue>,
            mut nargs: Option<tokay::Dict>
        ) -> Result<tokay::vm::Accept, tokay::vm::Reject> {
            // The function's original name in Tokay
            let __function = concat!(stringify!(#name), "()");

            // Arguments
            #(#arguments)*

            // Body
            #body
        }
    };

    TokenStream::from(gen)
}

#[proc_macro]
pub fn tokay_token(input: TokenStream) -> TokenStream {
    let def = syn::parse_macro_input!(input as BuiltinDef);

    let name = def.name;

    // Token names must start with an upper-case letter or underscore
    if !{
        let ch = name.to_string().chars().next().unwrap();
        ch.is_uppercase() || ch == '_'
    } {
        return quote_spanned! {
            name.span() => compile_error!(
                "Token identifier must start with an upper-case letter or underscore"
            );
        }
        .into();
    }

    let function = syn::Ident::new(
        &name.to_string().to_lowercase(),
        proc_macro2::Span::call_site(),
    );
    let callable = syn::Ident::new(
        &format!("tokay_token_{}", name.to_string().to_lowercase()),
        proc_macro2::Span::call_site(),
    );

    // Generate assignment to identifier for each argument.
    let arguments = gen_assign_arguments(def.arguments);
    let body = def.body;

    // Generate function and wrapper
    let gen = quote! {
        pub fn #function(
            context: &mut tokay::vm::Context,
            mut args: Vec<tokay::RefValue>,
            mut nargs: Option<tokay::Dict>
        ) -> Result<tokay::Accept, tokay::Reject> {
            // The function's original name in Tokay
            let __function = concat!(stringify!(#name), "()");

            // Arguments
            #(#arguments)*

            // Body
            #body
        }

        pub fn #callable(
            context: Option<&mut tokay::Context>,
            args: Vec<tokay::RefValue>,
            nargs: Option<tokay::Dict>
        ) -> Result<tokay::Accept, tokay::Reject> {
            #function(context.unwrap(), args, nargs)
        }
    };

    TokenStream::from(gen)
}

#[proc_macro]
pub fn tokay_tests(input: TokenStream) -> TokenStream {
    let pattern = syn::parse_macro_input!(input as syn::LitStr);
    let pattern = pattern.value();

    let mut tests = Vec::new();

    for test in glob(&pattern).expect(&format!("Failed to read {:?}", pattern)) {
        let test = test.unwrap();
        let name = format!("test_{}", test.file_stem().unwrap().to_str().unwrap());
        let name = syn::Ident::new(&name, proc_macro2::Span::call_site());
        let path = test.to_str().unwrap();

        tests.push(TokenStream::from(quote!(
            #[test]
            fn #name() {
                crate::utils::testcase(#path);
            }
        )));
    }

    //println!("tests = {:#?}", tests);

    return TokenStream::from_iter(tests.into_iter());
}
