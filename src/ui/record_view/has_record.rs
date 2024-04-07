use crate::{
    model::{record::Record, tree::RecordNode},
    search::item::SearchMatch,
};
use gtk::{glib, glib::prelude::*};

pub trait HasRecord: Send + Sync {
    fn get_type(&self) -> glib::types::Type;
    fn get_record<'a>(&self, obj: &'a glib::Object) -> &'a Record;
}

struct RecordNodeHasRecord;

impl HasRecord for RecordNodeHasRecord {
    fn get_type(&self) -> glib::types::Type {
        RecordNode::static_type()
    }

    fn get_record<'a>(&self, obj: &'a glib::Object) -> &'a Record {
        obj.downcast_ref::<RecordNode>().unwrap().record()
    }
}

pub static RECORD_NODE_HAS_RECORD: &dyn HasRecord = &RecordNodeHasRecord;

struct SearchMatchHasRecord;

impl HasRecord for SearchMatchHasRecord {
    fn get_type(&self) -> glib::types::Type {
        SearchMatch::static_type()
    }

    fn get_record<'a>(&self, obj: &'a glib::Object) -> &'a Record {
        obj.downcast_ref::<SearchMatch>().unwrap().record().record()
    }
}

pub static SEARCH_MATCH_HAS_RECORD: &dyn HasRecord = &SearchMatchHasRecord;

#[derive(Clone, Copy)]
pub struct PSRecordViewOptions {
    pub has_record: &'static dyn HasRecord,
    pub drag_and_drop: bool,
}
