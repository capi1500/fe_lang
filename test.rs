fn make_incrementor<'a>(r: &'a mut i32) -> impl FnMut() + 'a {
	move || {
		*r += 1;
	}
}

fn h() {
	let mut x = 5;
	let mut f = make_incrementor(&mut x);
	f();
	drop(f); // bez drop nie działa, bo f ma explicit lifetime 'a==lifetime x
	println!("{}", &x);
}

fn g() {
	let mut x = 5;
	let mut f = || {
		x += 1;
	}; // tutaj f ma któtszy lifetime niż x
	f();
	println!("{}", &x);
}

struct Foo<'a, 'b> {
	x: &'a i32,
	y: &'b i32,
}

// fn i() {
// 	let x = 5;
// 	let y = 4;
// 	let foo = i_helper(&x, &y);
// 	println!("{} {}", foo.x, foo.y);
// }

fn j() {
	struct Foo {
		a: i32
	}
	let mut x = Foo {a: 5};
	let mut foo = || {
		x.a += 2;
		x.a
	};
	println!("{}", foo());
}

// fn k() {
// 	let mut x1 = 1;
// 	let mut x2 = 2;
// 	let mut z = if true { &mut x1 } else { &mut x2 };
// 	let mut tmp = &mut x1;
// 	println!("{}", z);
// }

fn k() {
	let mut x1 = 1;
	let mut x2 = 2;
	let mut x3 = 3;
	let mut y1 = &mut x1;
	let mut y2 = &mut x2;
	// *(if true { y1 } else { y2 }) = x3;
	*(if true { &mut y1 } else { &mut y2 }) = &mut x3;
	x3 = 4;
	// println!("{}", y1);
	// println!("{}", y2);
	println!("{} {}", x1, x2);
}

fn l() {
	let mut x = 1;
	let mut y = 2;
	let mut z = &mut x;
	*(&mut z) = &mut y;
	println!("{}", z);
}

// fn m() {
// 	let mut x: &mut i32;
// 	let mut z = 3;
// 	{
// 		let mut y = 5;
// 		x = &mut y;
// 		println!("{}", x);
// 	}
// 	println!("{}", x);
// }

// fn n() {
// 	let mut a = 1;
// 	let mut b = &mut a;
// 	let mut c = &mut b;
// 	let mut d = *c;
// 	println!("{}", *d);
// }

/*
error[E0507]: cannot move out of `*c` which is behind a mutable reference
  --> test.rs:93:14
   |
93 |     let mut d = *c;
   |                 ^^
   |                 |
   |                 move occurs because `*c` has type `&mut i32`, which does not implement the `Copy` trait
   |                 help: consider borrowing here: `&*c`
   */

// fn m() {
// 	let mut a1 = 1;
// 	let mut a2 = 2;
// 	let mut b1 = &mut a1;
// 	let mut b2 = &mut a2;
// 	let mut c = if true {&mut b1} else {&mut b2};
// 	let mut d = **c; // *c == b, witch is moved out here
// 	println!("{}", d);
// }

/*
error[E0507]: cannot move out of `*c` which is behind a mutable reference
  --> test.rs:93:14
    |
114 |     let mut d = *c;
    |                 ^^
    |                 |
    |                 move occurs because `*c` has type `&mut i32`, which does not implement the `Copy` trait
    |                 help: consider borrowing here: `&*c`
   */

// fn o() {
// 	let a: &i32;
// 	let z: &&i32 = {
// 		let n = 5;
// 		a = &n;
// 		&a
// 	};
// 	println!("{}", **z)
// }

fn p() {
	let mut a = 1;
	*(&mut a);
}

fn q1(a: &mut [i32]) {
	//a = [4,5,6]; // invalid
	a[1] = 5;
	a[2] = 6;
	println!("{:?}", a);
}

fn q2() {
	let mut a = [1, 2, 3];
	q1(&mut a);
	println!("{:?}", a);
}

fn r1(a: &mut i32) {
	// a = 4; invalid
	*a = 4;
	println!("{}", a);
}

fn r2() {
	let mut a = 1;
	let mut b = &mut a;
	r1(b);
	r1(b);
	r1(b);
	r1(b);
	println!("{}", a);
}

fn s1(row: &[i32]) -> usize {
	return row.len();
}

fn s2() {
	let arr = [1,2,3];
	let r = &arr;
	let x = r;
	let y = r;
	println!("{:?} {:?} {:?}", r, x, y);
}

fn main() {
	s2();
}
