use super::record::Record;

pub type RecordTree = Box<Vec<RecordNode>>;

#[derive(Debug, Clone)]
pub enum RecordNode {
    Group(Record, RecordTree),
    Leaf(Record),
}
