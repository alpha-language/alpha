/// Give write in `dest` the `src` and return it
#[inline]
pub fn move_to<T>(dest: &mut T, src: T) -> T {
  // SAFETY: We write into `dest` afterwards,
  // we read `dest` to get the current value
  unsafe {
    core::ptr::write(dest, src);
    core::ptr::read(dest)
  }
}
