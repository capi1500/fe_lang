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

fn i() {
	#[derive(Debug)]
	struct Foo {
		x: i32
	}
	
	fn foo(a: Foo) {
		println!("Do stuff {:?}", a);
	}
	
	foo(Foo{
		x: 5
	});
	
	struct Foo {
		y: i32
	}
	
	let a: Foo {
		y: 6
	};
	
	println!("{}", a.y);
}

fn main() {
	i();
}
