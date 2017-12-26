#[no_mangle]
pub extern fn spinlock(n: usize, skip: usize) -> i32 {
    let mut buffer: Vec<i32> = Vec::with_capacity(n+1);
    buffer.push(0);
    buffer.push(1);
    let mut pos = 1;

    for i in 2..n+1 {
        pos = (pos + skip + 1) % buffer.len();
        buffer.insert(pos, i as i32);
    }

    pos = (pos + 1) % buffer.len();

    return buffer[pos];
}

#[no_mangle]
pub extern fn spinlock0(n: usize, skip: usize) -> i32 {
    let mut pos = 1;
    let mut pos_0 = 0;
    let mut after_0 = 1;

    for i in 2..n+1 {
        pos = (pos + skip + 1) % i;
        if pos == pos_0 + 1 {
            after_0 = i;
        }
        if pos <= pos_0 {
            pos_0 += 1;
        }
    }

    return after_0 as i32;
}
