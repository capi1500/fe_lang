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

fn main() {
	l();
}
