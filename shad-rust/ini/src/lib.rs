#![forbid(unsafe_code)]

use std::collections::HashMap;

////////////////////////////////////////////////////////////////////////////////

pub type IniFile = HashMap<String, HashMap<String, String>>;

pub fn parse(content: &str) -> IniFile {
    let mut result: HashMap<String, HashMap<String, String>> = HashMap::new();
    let mut current_title: Option<String> = None;
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            let title = trimmed.replacen('[', "", 1).replacen(']', "", 1);
            if title.contains(['[', ']']) {
                panic!("bad brackets");
            }
            current_title = Some(title);
            if let Some(ref ct) = current_title {
                let prev = result.insert(ct.to_string(), HashMap::new());
                if let Some(p) = prev {
                    result.insert(ct.to_string(), p);
                }
            }
        } else if !line.trim().is_empty() {
            if line.contains(['[', ']']) {
                panic!("bad brackets");
            }
            let mut parts = line.trim().split('=');
            let key = parts.next().unwrap_or("");
            let value = parts.next().unwrap_or("");
            if parts.next().is_some() {
                panic!("too many '='");
            }
            if let Some(ref ct) = current_title {
                result
                    .get_mut(ct)
                    .unwrap()
                    .insert(key.trim().to_string(), value.trim().to_string());
            } else {
                panic!("keys without title");
            }
        }
    }
    result
}
