-----------------------------------------------------------------------
--  babel_harness -- Unit test program for babel
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

with Babel.Testsuite;
with Util.Tests;

procedure Babel_Harness is

   procedure Harness is new Util.Tests.Harness (Babel.Testsuite.Suite);
begin
   Harness ("babel-tests.xml");
end Babel_Harness;
