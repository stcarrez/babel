-----------------------------------------------------------------------
--  babel-stores-local-tests - Unit tests for babel streams
--  Copyright (C) 2016 Stephane Carrez
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
with Util.Test_Caller;
with Util.Encoders.SHA1;
with Util.Encoders.Base16;
with Babel.Files.Signatures;

package body Babel.Stores.Local.Tests is

   package Caller is new Util.Test_Caller (Test, "Stores.Local");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Babel.Streams.Read",
                       Test_Read_File'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the Read_File and Write_File operations.
   --  ------------------------------
   procedure Test_Read_File (T : in out Test) is
      Buffers   : aliased Babel.Files.Buffers.Buffer_Pool;
      Local     : aliased Babel.Stores.Local.Local_Store_Type;
      Stream    : Babel.Streams.Refs.Stream_Ref;
      Sign      : Util.Encoders.SHA1.Hash_Array;
      Sign_Copy : Util.Encoders.SHA1.Hash_Array;
      Hex       : Util.Encoders.Base16.Encoder;
   begin
      Buffers.Create_Pool (Size  => 16 * 1024,
                           Count => 10_000);
      Local.Set_Buffers (Buffers'Unchecked_Access);
      Local.Set_Root_Directory (Util.Tests.Get_Path (""));
      Local.Read_File ("bin/babel_harness", Stream);
      Local.Write_File ("regtests/result/babel_harness.copy", Stream, 8#755#);
      Babel.Files.Signatures.Sha1 (Stream, Sign);
      Local.Read_File ("regtests/result/babel_harness.copy", Stream);
      Babel.Files.Signatures.Sha1 (Stream, Sign_Copy);
      declare
         S1 : constant String := Util.Encoders.Transform (Hex, Sign);
         S2 : constant String := Util.Encoders.Transform (Hex, Sign_Copy);
      begin
         Util.Tests.Assert_Equals (T, S1, S2, "SHA1 comparison");
      end;
   end Test_Read_File;

end Babel.Stores.Local.Tests;
