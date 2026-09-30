/*
 * Copyright (C)2005-2026 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

import sa.Boot;

@:keepInit
@:coreApi class Std {
	@:deprecated('Std.is is deprecated. Use Std.isOfType instead.')
	public static inline function is(v:Dynamic, t:Dynamic):Bool {
		return isOfType(v, t);
	}

	public static inline function isOfType(v:Dynamic, t:Dynamic):Bool {
		return untyped sa.Boot.__instanceof(v, t);
	}

	public static inline function downcast<T:{}, S:T>(value:T, c:Class<S>):Null<S> {
		return untyped sa.Boot.__instanceof(value, c) ? cast value : null;
	}

	@:deprecated('Std.instance() is deprecated. Use Std.downcast() instead.')
	public static inline function instance<T:{}, S:T>(value:T, c:Class<S>):Null<S> {
		return downcast(value, c);
	}

	@:keep
	public static function string(s:Dynamic):String {
		// v0.1: lowered to sa_std/fmt.sai buffer ops (see SA_TARGET.md).
		return sa.Boot.stringify(s);
	}

	public static function int(x:Float):Int {
		if (!Math.isFinite(x) || Math.isNaN(x))
			return 0;
		else
			return sa.Boot.clampInt32(x);
	}

	public static function parseInt(x:String):Null<Int> {
		// v0.1 stub: full parsing lowers to sa_std string/fmt ops (see SA_TARGET.md).
		if (x == null)
			return null;
		return sa.Boot.parseIntPrefix(x);
	}

	public static function parseFloat(x:String):Float {
		// v0.1 stub: full parsing lowers to sa_std string/fmt ops (see SA_TARGET.md).
		if (x == null || x == "")
			return Math.NaN;
		return sa.Boot.parseFloatPrefix(x);
	}

	public static function random(x:Int):Int {
		return untyped x <= 0 ? 0 : Math.floor(Math.random() * x);
	}
}
