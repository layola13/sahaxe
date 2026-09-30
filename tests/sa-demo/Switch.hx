enum Color {
	Red;
	Green;
	Blue;
}

class Switch {
	static function main():Void {
		var c:Color = Color.Green;
		var n:Int = 0;
		switch (c) {
			case Red: n = 1;
			case Green | Blue: n = 2;
			default: n = 9;
		}
		var m:Int = 0;
		switch (n) {
			case 1: m = 10;
			case 2: m = 20;
		}
		if (m == 20) {
			trace("switch ok");
		} else {
			trace("switch bad");
		}
	}
}
