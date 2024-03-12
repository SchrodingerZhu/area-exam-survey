#![feature(let_chains)]
use std::rc::Rc;
/// ```hs
/// data Color = R | B | BB
/// data RBTree = Empty | Node Color RBTree Int RBTree
/// ```
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum Color {
    R,
    B,
    BB,
}

#[derive(Clone, Debug)]
pub enum RbTree {
    Empty,
    Node(Color, Rc<RbTree>, i32, Rc<RbTree>),
}

use Color::*;
use RbTree::*;
/// balance B (Node R (Node R a x b) y c) z d = Node R (Node B a x b) y (Node B c z d)
/// balance B (Node R a x (Node R b y c)) z d = Node R (Node B a x b) y (Node B c z d)
/// balance B a x (Node R b y (Node R c z d)) = Node R (Node B a x b) y (Node B c z d)
/// balance B a x (Node R (Node R b y c) z d) = Node R (Node B a x b) y (Node B c z d)
/// balance color l k r = Node color l k r

pub fn balance(color: Color, left: Rc<RbTree>, value: i32, right: Rc<RbTree>) -> Rc<RbTree> {
    if color == B
        && let Node(R, ll, y, c) = &*left
        && let Node(R, a, x, b) = &**ll
    {
        return Rc::new(Node(
            R,
            Rc::new(Node(B, a.clone(), *x, b.clone())),
            *y,
            Rc::new(Node(B, c.clone(), value, right)),
        ));
    }
    todo!("remaining balance cases")
}
