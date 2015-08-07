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

with Util.Test_Caller;
with Babel.Files.Buffers;
with Babel.Streams.Cached;
with Babel.Streams.Files;
with Babel.Streams.XZ;
package body Babel.Streams.Tests is

   package Caller is new Util.Test_Caller (Test, "Streams");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Babel.Streams.Read",
                       Test_Stream_Composition'Access);
   end Add_Tests;

   --  ------------------------------
   --  Stream copy, compression and decompression test.
   --  Create a compressed version of the source file and then decompress the result.
   --  The source file is then compared to the decompressed result and must match.
   --  ------------------------------
   procedure Do_Copy (T    : in out Test;
                      Pool : in out Babel.Files.Buffers.Buffer_Pool;
                      Src  : in String) is
      use type Babel.Files.Buffers.Buffer_Access;

      Src_Path : constant String := Util.Tests.Get_Path (Src);
      Dst_Path : constant String := Util.Tests.Get_Test_Path ("regtests/result/" & Src & ".xz");
      Tst_Path : constant String := Util.Tests.Get_Test_Path ("regtests/result/" & Src);
      Buffer   : Babel.Files.Buffers.Buffer_Access;
   begin
      --  Compress the 'configure' file into 'configure.xz' through the file+cache+xz+file streams.
      declare
         In_File  : aliased Babel.Streams.Files.Stream_Type;
         Out_File : aliased Babel.Streams.Files.Stream_Type;
         Cache    : aliased Babel.Streams.Cached.Stream_Type;
         Lz       : aliased Babel.Streams.XZ.Stream_Type;
      begin
         Pool.Get_Buffer (Buffer);
         In_File.Open (Src_Path, Buffer);
         Cache.Load (In_File, Pool);

         Pool.Get_Buffer (Buffer);
         Out_File.Create (Dst_Path, 8#644#);
         Lz.Set_Buffer (Buffer);
         Lz.Set_Output (Out_File'Unchecked_Access);
         loop
            Cache.Read (Buffer);
            exit when Buffer = null;
            Lz.Write (Buffer);
         end loop;
         Lz.Flush;
         Lz.Close;
      end;

      --  Decompress through file+cache+xz+file
      declare
         In_File  : aliased Babel.Streams.Files.Stream_Type;
         Out_File : aliased Babel.Streams.Files.Stream_Type;
         Cache    : aliased Babel.Streams.Cached.Stream_Type;
         Lz       : aliased Babel.Streams.XZ.Stream_Type;
      begin
         Pool.Get_Buffer (Buffer);
         In_File.Open (Dst_Path, Buffer);
         Cache.Load (In_File, Pool);

         --  Setup decompression.
         Pool.Get_Buffer (Buffer);
         Lz.Set_Input (Cache'Unchecked_Access);
         Lz.Set_Buffer (Buffer);

         Out_File.Create (Tst_Path, 8#644#);
         loop
            Lz.Read (Buffer);
            exit when Buffer = null;
            Out_File.Write (Buffer);
         end loop;
         Out_File.Close;
      end;
      Util.Tests.Assert_Equal_Files (T, Src_Path, Tst_Path,
                                     "Composition stream failed for: " & Src);

   end Do_Copy;

   --  ------------------------------
   --  Test the Find function resolving some existing user.
   --  ------------------------------
   procedure Test_Stream_Composition (T : in out Test) is
      Pool     : aliased Babel.Files.Buffers.Buffer_Pool;
   begin
      Babel.Files.Buffers.Create_Pool (Into  => Pool,
                                       Size  => 1_000,
                                       Count => 1000);
      Do_Copy (T, Pool, "configure");
      Do_Copy (T, Pool, "babel.gpr");
      Do_Copy (T, Pool, "configure.in");
      Do_Copy (T, Pool, "config.guess");
      Do_Copy (T, Pool, "Makefile.in");
   end Test_Stream_Composition;

end Babel.Streams.Tests;
