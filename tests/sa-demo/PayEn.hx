enum Shape {
	Circle(r:Float);
	Rect(w:Int, h:Int);
	Dot;
}

class PayEn {
	static function area(s:Shape):Float {
		return switch (s) {
			case Circle(r): 3.14 * r * r;
			case Rect(w, h): w * h * 1.0;
			case Dot: 0.0;
		}
	}

	static function main():Void {
		var a:Shape = Circle(2.0);
		if (area(a) > 12.0) {
			trace("pay ok");
		} else {
			trace("pay bad");
		}
	}
}
