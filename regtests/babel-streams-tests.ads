-----------------------------------------------------------------------
--  babel-streams-tests - Unit tests for babel streams
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

with Util.Tests;

package Babel.Streams.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test the Find function resolving some existing user.
   procedure Test_Stream_Composition (T : in out Test);

   --  Stream copy, compression and decompression test.
   --  Create a compressed version of the source file and then decompress the result.
   --  The source file is then compared to the decompressed result and must match.
   procedure Do_Copy (T    : in out Test;
                      Pool : in out Babel.Files.Buffers.Buffer_Pool;
                      Src  : in String);

end Babel.Streams.Tests;
