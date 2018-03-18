use std::mem::size_of;
use serde::ser::Serialize;
use serde::de::DeserializeOwned;
use libc::{malloc, memcpy, free, c_void};
use bincode;
use gobject_sys::{g_object_set_data_full, g_object_get_data};
use glib::{Object, IsA};
use error::*;

unsafe fn buf_to_ptr(buffer: &[u8]) -> *mut c_void {
    let len = buffer.len();
    let prefix_len = size_of::<usize>();
    let ptr = malloc(prefix_len + len);
    *(ptr as *mut usize) = len;
    memcpy(ptr.offset(prefix_len as isize), buffer.as_ptr() as *const c_void, len);
    ptr
}

unsafe fn ptr_to_buf(ptr: *const c_void) -> Vec<u8> {
    let mut buffer = Vec::<u8>::new();
    let len = *(ptr as *const usize);
    buffer.resize(len, 0u8);
    let prefix_len = size_of::<usize>();
    memcpy(buffer.as_ptr() as *mut c_void, ptr.offset(prefix_len as isize), len);
    buffer
}

pub fn object_set_data<O, T>(obj: &O, key: &str, value: &T) -> Result<()>
    where
        O: IsA<Object>,
        T: Serialize
{
    let buffer = bincode::serialize(value)?;

    let ckey = ::std::ffi::CString::new(key).unwrap();
    unsafe {
        let ptr = buf_to_ptr(&buffer);
        g_object_set_data_full(obj.to_glib_none().0, ckey.as_ptr(), ptr, Some(free));
    }
    Ok(())
}

pub fn object_get_data<O, T>(obj: &O, key: &str) -> Result<T>
    where
        O: IsA<Object>,
        T: DeserializeOwned
{
    let ckey = ::std::ffi::CString::new(key).unwrap();
    let buffer = unsafe {
        let ptr = g_object_get_data(obj.to_glib_none().0, ckey.as_ptr());
        ptr_to_buf(ptr)
    };
    let value = bincode::deserialize(&buffer)?;
    Ok(value)
}