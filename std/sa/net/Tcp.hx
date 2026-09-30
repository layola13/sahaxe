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

package sa.net;

/**
	Minimal TCP surface over `sci/sa_std` net contracts (v0.18).

	Handles are opaque `UInt` values (64-bit slots, mirroring the ts
	plugin's i32-handle convention widened to full width). All calls
	lower directly to `NET_TCP_*` macros / `sa_std_net_*` externs;
	failures abort loudly via `panic` (same posture as fs).
**/
extern class Tcp {
	/** Bind 127.0.0.1:port (0 = ephemeral), returns listener handle. */
	public static function listen(port:Int):UInt;

	/** Bound port of a listener created with port 0. */
	public static function boundPort(listener:UInt):Int;

	/** Close a listener. */
	public static function close(listener:UInt):Void;
}
