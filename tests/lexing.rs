use std::{
    fmt::format,
    fs::{self, FileType},
    path::{Path, PathBuf},
    rc,
};

#[test]
fn lexing_regression() {
    let files = get_files();

    for f in files {
        let mut path = f.1;
        path.set_extension("lexing");

        let new = rcc::lexer::Lexer::new(f.0.into()).all_tokens();
        let new = format!("{}", new);

        if path.exists() {
            let old = fs::read_to_string(&path).unwrap();
            if old != new {
                path.set_extension("lexing-diff");
                fs::write(&path, new).unwrap();
                panic!("old/ new did not match. {:?}", path);
            }
            assert_eq!(old, new);
        } else {
            fs::write(path, new).unwrap();
        }
    }
}

pub fn get_files() -> impl Iterator<Item = (String, PathBuf)> {
    for f in fs::read_dir("tests/c_src/").unwrap() {
        let path = f.unwrap().path();
        if path.extension().unwrap() == "lexing-diff" {
            fs::remove_file(path).unwrap();
        }
    }

    fs::read_dir("tests/c_src/")
        .unwrap()
        .filter(|p| p.as_ref().unwrap().path().extension().unwrap() == "c")
        .map(|path| {
            let path = path.unwrap().path();

            (fs::read_to_string(&path).unwrap(), path)
        })
}
