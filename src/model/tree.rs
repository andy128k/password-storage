use super::record::Record;

#[derive(Debug, Clone)]
pub enum RecordNode {
    Group(Record, Vec<RecordNode>),
    Leaf(Record),
}

#[derive(Debug, Clone)]
pub struct RecordTree {
    pub records: Vec<RecordNode>,
}
