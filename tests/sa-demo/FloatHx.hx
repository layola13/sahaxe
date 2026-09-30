class FloatHx {
	static function area(r:Float):Float {
		return 3.14 * r * r;
	}

	static function main():Void {
		var a:Float = 1.5;
		var b:Float = 2.25;
		var c:Float = a + b * 2.0;
		var d:Float = -c;
		var e:Float = area(10.0);
		var n:Int = Std.int(9.99);
		var m:Float = n + 0.01;
		if (c > 5.0 && d < 0.0 && e > 300.0 && n == 9 && m > 9.0) {
			trace("float ok");
		} else {
			trace("float bad");
		}
		trace(Std.string(1.5));
	}
}
