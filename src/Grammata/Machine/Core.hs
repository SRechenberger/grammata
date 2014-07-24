{-|
Module : Grammata.Machine.Core
Description : Grammata Core Language Module
Maintainer : sascha.rechenberger@uni-ulm.de
Stability : stable
Portability : portable
Copyright : (c) Sascha Rechenberger, 2014
License : GPL-3

This file is part of grammata.

grammata is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

grammata is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with grammata. If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Grammata.Machine.Core
(
    -- * Submodules
    module Grammata.Machine.Core.Expression,
    module Grammata.Machine.Core.Functional,
    module Grammata.Machine.Core.Imperative,
    module Grammata.Machine.Core.Logical,
)
where   

    import Grammata.Machine.Core.Expression 
    import Grammata.Machine.Core.Functional 
    import Grammata.Machine.Core.Imperative 
    import Grammata.Machine.Core.Logical 