use glib::glib_sys;
use glib::gobject_sys;
use glib::{
    translate::{ToGlib, ToGlibPtr},
    ObjectExt, Quark,
};

pub trait ObjectDataExt {
    unsafe fn set_qdata<QD: 'static>(&self, key: Quark, value: QD);
    unsafe fn get_qdata<QD: 'static>(&self, key: Quark) -> Option<&QD>;
    unsafe fn steal_qdata<QD: 'static>(&self, key: Quark) -> Option<QD>;

    unsafe fn set_data<QD: 'static>(&self, key: &str, value: QD) {
        self.set_qdata::<QD>(Quark::from_string(key), value)
    }

    unsafe fn get_data<QD: 'static>(&self, key: &str) -> Option<&QD> {
        self.get_qdata::<QD>(Quark::from_string(key))
    }

    unsafe fn steal_data<QD: 'static>(&self, key: &str) -> Option<QD> {
        self.steal_qdata::<QD>(Quark::from_string(key))
    }
}

impl<O> ObjectDataExt for O
where
    O: ObjectExt,
{
    unsafe fn set_qdata<QD: 'static>(&self, key: Quark, value: QD) {
        unsafe extern "C" fn drop_value<QD>(ptr: glib_sys::gpointer) {
            debug_assert!(!ptr.is_null());
            let value: Box<QD> = Box::from_raw(ptr as *mut QD);
            drop(value)
        }

        let ptr = Box::into_raw(Box::new(value)) as glib_sys::gpointer;
        gobject_sys::g_object_set_qdata_full(
            self.as_object_ref().to_glib_none().0,
            key.to_glib(),
            ptr,
            Some(drop_value::<QD>),
        );
    }

    unsafe fn get_qdata<QD: 'static>(&self, key: Quark) -> Option<&QD> {
        let ptr =
            gobject_sys::g_object_get_qdata(self.as_object_ref().to_glib_none().0, key.to_glib());
        if ptr.is_null() {
            None
        } else {
            Some(&*(ptr as *const QD))
        }
    }

    unsafe fn steal_qdata<QD: 'static>(&self, key: Quark) -> Option<QD> {
        let ptr =
            gobject_sys::g_object_steal_qdata(self.as_object_ref().to_glib_none().0, key.to_glib());
        if ptr.is_null() {
            None
        } else {
            let value: Box<QD> = Box::from_raw(ptr as *mut QD);
            Some(*value)
        }
    }
}
