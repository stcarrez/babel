-----------------------------------------------------------------------
--  babel-base-text-tests - Unit tests for babel text database
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

with Util.Test_Caller;

package body Babel.Base.Text.Tests is

   package Caller is new Util.Test_Caller (Test, "Base.Users");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Babel.Base.Text.Load",
                       Test_Load'Access);
   end Add_Tests;

   --  Test the loading a text database.
   procedure Test_Load (T : in out Test) is
      DB : Text_Database;
   begin
      DB.Load (Util.Tests.Get_Path ("regtests/files/db/database.txt"));
   end Test_Load;

end Babel.Base.Text.Tests;
