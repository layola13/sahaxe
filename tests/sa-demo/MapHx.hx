class MapHx {
	static function main():Void {
		var m:Map<String, Int> = new Map();
		m.set("a", 1);
		m.set("b", 2);
		m.set("a", 10);
		var v:Int = m.get("a");
		if (v == 10) {
			trace("get ok");
		} else {
			trace("get bad");
		}
		if (m.exists("b") && !m.exists("z")) {
			trace("exists ok");
		} else {
			trace("exists bad");
		}
		var r:Bool = m.remove("b");
		if (r && !m.exists("b") && m.get("zzz") == 0) {
			trace("remove ok");
		} else {
			trace("remove bad");
		}
	}
}
