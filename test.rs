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

fn j() {
	struct Foo {
		a: i32
	}
	let mut x = Foo {a: 5};
	let mut foo = || {
		x.a += 2;
		x.a
	};
	println!("{}", x.a);
	println!("{}", foo());
}

fn main() {
	j();
}
