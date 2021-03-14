use std::alloc::{GlobalAlloc, Layout};
use std::ffi::c_void;

mod ffi;

pub struct BoehmAllocator;

impl BoehmAllocator {
    pub fn init() {
        unsafe { ffi::GC_init() }
    }
}

unsafe impl GlobalAlloc for BoehmAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        ffi::GC_malloc(layout.size() as u64) as *mut u8
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {}

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        self.alloc(layout)
    }

    unsafe fn realloc(&self, ptr: *mut u8, _layout: Layout, new_size: usize) -> *mut u8 {
        ffi::GC_realloc(ptr as *mut c_void, new_size as u64) as *mut u8
    }
}
