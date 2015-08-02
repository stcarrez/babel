-----------------------------------------------------------------------
--  babel-base-users-tests - Unit tests for babel users
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

package body Babel.Base.Users.Tests is

   use type Util.Strings.Name_Access;

   package Caller is new Util.Test_Caller (Test, "Base.Users");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Babel.Base.Users.Find",
                       Test_Find'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test the Find function resolving some existing user.
   --  ------------------------------
   procedure Test_Find (T : in out Test) is
      Db   : Babel.Base.Users.Database;
      User : User_Type;
   begin
      User := Db.Find (0, 0);
      T.Assert (User.Name /= null, "User uid=0 was not found");
      T.Assert (User.Group /= null, "User gid=0 was not found");
      Util.Tests.Assert_Equals (T, "root", User.Name.all, "Invalid root user name");
      Util.Tests.Assert_Equals (T, "root", User.Group.all, "Invalid root group name");

      User := Db.Find ("bin", "daemon");
      T.Assert (User.Name /= null, "User bin was not found");
      T.Assert (User.Group /= null, "Group daemon was not found");
      Util.Tests.Assert_Equals (T, "bin", User.Name.all, "Invalid 'bin' user name");
      Util.Tests.Assert_Equals (T, "daemon", User.Group.all, "Invalid 'daemon' group name");
      T.Assert (User.Uid > 0, "Invalid 'bin' uid");
      T.Assert (User.Gid > 0, "Invalid 'daemon' gid");
   end Test_Find;

end Babel.Base.Users.Tests;
