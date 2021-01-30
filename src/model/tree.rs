use super::record::Record;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecordNode {
    Group(Record, Vec<RecordNode>),
    Leaf(Record),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordTree {
    pub records: Vec<RecordNode>,
}
