enum Shape {
	Circle(r:Float);
	Rect(w:Int, h:Int);
	Dot;
}

class EnumEq {
	static function same(a:Shape, b:Shape):Bool {
		return a == b;
	}

	static function main():Void {
		var a:Shape = Circle(2.0);
		var b:Shape = Circle(2.0);
		var c:Shape = Circle(3.0);
		var d:Shape = Rect(2, 3);
		var e:Shape = Dot;
		var f:Shape = Dot;
		if (same(a, b) && !(a == c) && !(a == d) && (e == f)) {
			trace("enumeq ok");
		} else {
			trace("enumeq bad");
		}
		if (a != c) {
			trace("neq ok");
		} else {
			trace("neq bad");
		}
	}
}
