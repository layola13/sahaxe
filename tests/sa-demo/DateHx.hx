class DateHx {
	static function main():Void {
		var d = Date.now();
		var t:Float = d.getTime();
		var y:Int = d.getFullYear();
		var mo:Int = d.getMonth();
		if (t > 0.0 && y > 2020 && mo >= 0 && mo <= 11) {
			trace("date ok");
		} else {
			trace("date bad");
		}
	}
}
