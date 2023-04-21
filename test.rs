// 4
fn foo(x: i32) {
	println!("{}", x)
}

// 2
fn bar() -> Box<dyn Fn(i32) -> i32> {
	let x = 5;
	let foo = move |y| x + y;
	return Box::new(foo);
}

// fn bis() -> Box<dyn FnMut()> {
// 	let mut x = 5;
// 	let foo = || {
// 		x += 1;
// 		println!("{}", x);
// 	};


// --> test.rs:15:12
// 	|
//  15 |     let foo = || {
// 	|               ^^ may outlive borrowed value `x`
//  16 |         x += 1;
// 	|         - `x` is borrowed here
// 	|
//  note: closure is returned here
//    --> test.rs:19:9
// 	|
//  19 |     return Box::new(foo);
// 	|            ^^^^^^^^^^^^^
//  help: to force the closure to take ownership of `x` (and any other referenced variables), use the `move` keyword
// 	|
//  15 |     let foo = move || {
// 	|               ++++
 
//  error: aborting due to previous error


// 	return Box::new(foo);
// }

fn main() {
	// 4
	foo({
		let a = 4;
		a * 2
	});
	
	// 2
	let f = bar();
	println!("{}", f(1));
}
