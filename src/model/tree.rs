use super::record::Record;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecordNode {
    Group(Record, Vec<RecordNode>),
    Leaf(Record),
}

impl RecordNode {
    pub fn record(&self) -> &Record {
        match self {
            Self::Group(ref record, ..) => record,
            Self::Leaf(ref record) => record,
        }
    }

    pub fn children(&self) -> &[Self] {
        match self {
            Self::Group(.., ref nodes) => nodes,
            Self::Leaf(..) => &[],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordTree {
    pub records: Vec<RecordNode>,
}
