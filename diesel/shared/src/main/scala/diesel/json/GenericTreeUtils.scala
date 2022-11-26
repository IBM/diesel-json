/*
 * Copyright 2018 The Diesel Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package diesel.json

import diesel.GenericNode

object GenericTreeUtils {

  def findInParents(node: GenericNode, p: GenericNode => Boolean, includeNode: Boolean = false): Option[GenericNode] = {
    if (includeNode) {
      findInParents(node, p)
    } else {
      node.parent.flatMap(parent => findInParents(parent, p))
    }
  }

  private def findInParents(node: GenericNode, p: GenericNode => Boolean): Option[GenericNode] =
    if (p(node)) {
      Some(node)
    } else {
      node.parent.flatMap(parent => findInParents(parent, p))
    }

}
