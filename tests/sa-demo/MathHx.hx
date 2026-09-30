class MathHx {
	static function main():Void {
		var f:Int = Math.floor(1.9);
		var c:Int = Math.ceil(1.1);
		var r:Int = Math.round(2.5);
		var s:Float = Math.sqrt(16.0);
		var p:Float = Math.pow(2.0, 10.0);
		var si:Float = Math.sin(0.0);
		var m:Float = Math.random();
		if (f == 1 && c == 2 && r == 3 && s == 4.0 && p == 1024.0 && si == 0.0) {
			trace("math ok");
		} else {
			trace("math bad");
		}
		if (m >= 0.0 && m < 1.0) {
			trace("rand ok");
		} else {
			trace("rand bad");
		}
		var pi:Float = Math.PI;
		if (pi > 3.14 && pi < 3.15) {
			trace("pi ok");
		} else {
			trace("pi bad");
		}
	}
}
