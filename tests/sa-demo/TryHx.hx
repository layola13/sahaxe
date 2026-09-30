class TryHx {
	static function boom():Void {
		throw "boom";
	}

	static function main():Void {
		var x:Int = 0;
		try {
			x = 42;
		} catch (e:Dynamic) {
			x = -1;
		}
		if (x == 42) {
			trace("try ok");
		} else {
			trace("try bad");
		}
	}
}
