#![allow(dead_code)]
#![allow(unused)]
use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
};

#[derive(Debug)]
pub enum TreeError {
    RootAlreadyExists,
    RootDoesNotExist,
    IndexOutOfBounds,
    NodeDoesNotExist,
    RemovingChildFormEmptyNode,
    CantRemoveNodeWithChildren,
}

pub enum DumpCallbackType {
    Head,
    Node(usize, Option<usize>),
}

// Declare a type for anonymous function
type DCB<T> = Box<dyn Fn(DumpCallbackType, Option<&T>) -> String>;

pub struct Tree<T> {
    elements: Vec<Option<RefCell<NodeRef<T>>>>,
    empties: Vec<usize>,
    uid: u64,
    dcb: Option<DCB<T>>,
}

#[derive(Debug)]
struct NodeRef<T> {
    value: T,
    _type: TreeNodeType,
    children: Option<Vec<usize>>,
    index_in_tree: usize,
    index_in_parent: Option<usize>,
    parent_index_in_tree: Option<usize>,
}

impl<T> AsMut<T> for NodeRef<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

fn print_type_of<T>(_: &T) -> String {
    format!("{}", std::any::type_name::<T>().split("::").last().unwrap())
}

impl<T: std::fmt::Debug> NodeRef<T> {
    fn new(_type: TreeNodeType, value: T) -> Self {
        match _type {
            TreeNodeType::Root => Self {
                value,
                _type,
                children: Some(Vec::new()),
                index_in_tree: 0,
                index_in_parent: None,
                parent_index_in_tree: None,
            },
            TreeNodeType::Child(parent_index_in_tree) => NodeRef {
                value,
                _type,
                children: Some(Vec::new()),
                index_in_tree: 0,
                index_in_parent: None,
                parent_index_in_tree: Some(parent_index_in_tree),
            },
        }
    }
    fn is_empty(&self) -> bool {
        self.children.is_none() || self.children.as_ref().unwrap().is_empty()
    }
    fn dump_structure(&self, tree: &Tree<T>, level: usize) {
        let head = match tree.dcb {
            Some(ref f) => f(
                DumpCallbackType::Node(self.index_in_tree, self.parent_index_in_tree),
                Some(&self.value),
            ),
            None => format!("{}[{:?}]", print_type_of(&self.value), self.index_in_tree),
        };
        println!(
            "{}> {} {}",
            tree.uid,
            std::iter::repeat("-")
                .take(level as usize)
                .collect::<String>(),
            head
        );
        if let Some(children) = &self.children {
            for child_index in children {
                tree.get_node_ref(*child_index, |child| {
                    Ok(child.dump_structure(tree, level + 1))
                });
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TreeNodeType {
    Root,
    /// Child with parent's index_in_tree
    Child(usize),
}

static mut TREE_COUNTER: u64 = 0;

impl<T> Tree<T>
where
    T: std::fmt::Debug,
{
    /// Plant a tree capable to hold nodes of type T
    pub fn new() -> Self {
        let uid = unsafe {
            TREE_COUNTER += 1;
            TREE_COUNTER
        };
        Tree {
            elements: Vec::new(),
            empties: Vec::new(),
            uid,
            dcb: None,
        }
    }
    pub fn len(&self) -> usize {
        #[cfg(test)]
        {
            if self.elements.len() < self.empties.len() {
                panic!(
                    "Tree is corrupted {:#?}, {:#?}",
                    self.elements, self.empties
                );
            }
        }
        self.elements.len() - self.empties.len()
    }
    pub fn capacity(&self) -> usize {
        self.elements.len()
    }
    pub fn get_parent_index(&self, child: usize) -> Option<usize> {
        self.get_node_ref(child, |f| Ok(f.parent_index_in_tree))
            .unwrap()
    }
    pub fn exists_node_at(&self, node_index: usize) -> bool {
        if node_index >= self.elements.len() {
            return false;
        }
        self.elements[node_index].is_some()
    }
    pub fn remove_node_and_children(&mut self, index_in_tree: usize) -> Result<Vec<T>, TreeError> {
        let mut result = Vec::new();

        let mut node_ref = self.remove_node_ex(index_in_tree, false)?;

        let mut children = node_ref.borrow_mut().children.take().unwrap();

        for child_index in children {
            result.append(&mut self.remove_node_and_children(child_index)?);
        }
        result.push(node_ref.value);
        Ok(result)
    }
    pub fn remove_node(&mut self, index_in_tree: usize) -> Result<T, TreeError> {
        match self.remove_node_ex(index_in_tree, true) {
            Ok(node_ref) => Ok(node_ref.value),
            Err(e) => Err(e),
        }
    }
    fn remove_node_ex(
        &mut self,
        index_in_tree: usize,
        prevent_orphans: bool,
    ) -> Result<NodeRef<T>, TreeError> {
        if index_in_tree >= self.elements.len() {
            return Err(TreeError::IndexOutOfBounds);
        }

        // Get parent this child
        let (parent_index_in_tree, index_in_parent) = self.get_node_ref(index_in_tree, |f| {
            if prevent_orphans && !f.is_empty() {
                return Err(TreeError::CantRemoveNodeWithChildren);
            }
            Ok((f.parent_index_in_tree, f.index_in_parent))
        })?;

        if parent_index_in_tree.is_some() {
            // Remove child from parent
            self.get_node_ref(parent_index_in_tree.unwrap(), |parent| {
                let children_len = parent.children.as_ref().unwrap().len();
                // Check if child is at the end of the list
                let index_in_parent = index_in_parent.unwrap();
                if children_len == 0 {
                    panic!("Removing child {index_in_parent} T{index_in_tree} from empty node T{}, {:?}",parent.index_in_tree,parent.children);                    
                } else if (index_in_parent + 1) == children_len {
                    parent.children.as_mut().unwrap().pop();
                } else {
                    // Swap child position to last position
                    //println!("Swapping {} with {}", index_in_parent, children_len-1 );
                    let was_index_in_parent = parent
                        .children
                        .as_mut()
                        .unwrap()
                        .swap_remove(index_in_parent);

                    let swapped_index_in_tree = parent.children.as_mut().unwrap()[index_in_parent].borrow();
                    // Update swapped child index                     
                    self.get_node_ref(*swapped_index_in_tree, |child| {
                        //println!(" Updateing {} (T{}) to {}", child.index_in_parent.unwrap() ,swapped_index_in_tree, index_in_parent);
                        child.index_in_parent = Some(index_in_parent);
                        Ok(())
                    });
                }
                Ok(())
            });
        }

        let node_is_last = index_in_tree == self.elements.len() - 1;

        //Add empty space to empties if node is not at last position
        #[cfg(test)]
        println!(
            "Removing node T{} from tree, was: {:#?}",
            index_in_tree, self.elements
        );
        if !node_is_last {
            self.empties.push(index_in_tree);
            // Move out from RefCell the node and return it
            let ret = self.elements[index_in_tree].take().unwrap().into_inner();
            #[cfg(test)]
            println!(
                "Removed node T{} from tree, is: {:#?}",
                index_in_tree, self.elements
            );
            Ok(ret)
        } else {
            //Now remove node from tree
            let ret = self.elements.pop().unwrap().unwrap().into_inner();
            self.check_and_shrink();
            #[cfg(test)]
            println!(
                "Popped Removed node T{} from tree, is: {:#?}",
                index_in_tree, self.elements
            );
            Ok(ret)
        }
    }

    fn check_and_shrink(&mut self) {
        let mut torem: Vec<usize> = Vec::new();
        while self.elements.last().is_some() && self.elements.last().unwrap().is_none() {
            self.elements.pop();
            torem.push(self.elements.len());
        }
        #[cfg(test)]
        println!("EPT Removing {:?} from {:?}", torem, self.empties);
        self.empties
            .retain(|i| !torem.iter().find(|x| **x == *i).is_some());
        #[cfg(test)]
        println!("EPT Removed {:?} from {:?}", torem, self.empties);
    }

    pub fn add_node<F>(&mut self, nodetype: TreeNodeType, mut f: F) -> Result<usize, TreeError>
    where
        F: Fn() -> T,
    {
        let (index_in_tree, index_in_parent, parent_index_in_tree) = match nodetype {
            TreeNodeType::Root => {
                if self.elements.len() > 0 {
                    return Err(TreeError::RootAlreadyExists);
                }
                let index_in_tree = match self.empties.pop() {
                    Some(index) => Some(index),
                    None => None,
                };
                (index_in_tree, Option::<usize>::None, Option::<usize>::None)
            }
            TreeNodeType::Child(parent_index_in_tree) => {
                if self.elements.len() == 0 {
                    return Err(TreeError::RootDoesNotExist);
                }

                let mut return_index_in_tree: Option<usize> = None;
                // Retrieve index from empties or create new one
                let index_in_tree = match self.empties.pop() {
                    Some(index) => {
                        return_index_in_tree = Some(index);
                        index
                    }
                    None => self.elements.len(),
                };

                let mut parent = self.elements[parent_index_in_tree]
                    .as_ref()
                    .unwrap()
                    .borrow_mut();

                let index_in_parent = parent.children.as_mut().unwrap().len();

                // Add child to parent
                parent.children.as_mut().unwrap().push(index_in_tree);

                (
                    return_index_in_tree,
                    Some(index_in_parent),
                    Some(parent_index_in_tree),
                )
            }
        };

        let node = f();

        let mut noder = NodeRef::new(nodetype, node);
        match index_in_tree {
            Some(index) => {
                noder.index_in_tree = index;
            }
            None => {
                noder.index_in_tree = self.elements.len();
            }
        }

        noder.index_in_parent = index_in_parent;
        noder.parent_index_in_tree = parent_index_in_tree;

        // Create new heap allocated NodeRef
        let noderef = RefCell::new(noder);

        match index_in_tree {
            Some(index) => {
                self.elements[index] = Some(noderef);
                Ok(index)
            }
            None => {
                self.elements.push(Some(noderef));
                Ok(self.elements.len() - 1)
            }
        }
    }

    /// Get noderef by index
    ///  Index = 0 is root
    fn get_node_ref<F, H>(&self, index_in_tree: usize, mut f: F) -> Result<H, TreeError>
    where
        F: FnMut(&mut NodeRef<T>) -> Result<H, TreeError>,
    {
        if index_in_tree >= self.elements.len() {
            return Err(TreeError::IndexOutOfBounds);
        }
        if let Some(node) = self.elements[index_in_tree].as_ref() {
            return f(&mut node.borrow_mut());
        } else {
            return Err(TreeError::NodeDoesNotExist);
        }
        let mut _ref = self.elements[index_in_tree].as_ref().unwrap().borrow_mut();
        f(&mut _ref)
    }

    /// Get node by index.
    ///  This method return a reference of undelcared type.
    ///  To retrieve node info use get_node_info instead.
    pub fn get_node_mut(&mut self, index_in_tree: usize) -> Result<&mut T, TreeError> {
        if index_in_tree >= self.elements.len() {
            return Err(TreeError::IndexOutOfBounds);
        }
        if let Some(node) = self.elements[index_in_tree].as_mut() {
            return Ok(&mut node.get_mut().value);
        } else {
            return Err(TreeError::NodeDoesNotExist);
        }
    }

    /// Get node extendend indexing info.
    ///  This method returns touple of (index_in_tree, index_in_parent, parent_index_in_tree)
    pub fn get_node_info(
        &self,
        index_in_tree: usize,
    ) -> Result<(usize, Option<usize>, Option<usize>), TreeError> {
        if index_in_tree >= self.elements.len() {
            return Err(TreeError::IndexOutOfBounds);
        }
        if let Some(node) = self.elements[index_in_tree].as_ref() {
            return Ok((
                node.borrow().index_in_tree,
                node.borrow().index_in_parent,
                node.borrow().parent_index_in_tree,
            ));
        } else {
            return Err(TreeError::NodeDoesNotExist);
        }
    }

    /// Get node by index.
    ///  Index = 0 means root
    pub fn get_node<F, H>(&self, index: usize, mut f: F) -> Result<H, TreeError>
    where
        F: FnMut(&mut T, Option<usize>) -> Result<H, TreeError>,
    {
        self.get_node_ref(index, |node| f(&mut node.value, node.parent_index_in_tree))
    }

    /// Iterate on children and
    ///  pass index_in_tree, index_in_parent to callback
    pub fn foreach_children<F>(&self, index_in_tree: usize, mut f: F) -> Result<(), TreeError>
    where
        F: FnMut(&mut T, usize, usize),
    {
        if index_in_tree >= self.elements.len() {
            return Err(TreeError::IndexOutOfBounds);
        }

        if let Some(node) = self.elements[index_in_tree].as_ref() {
            let mut node = node.borrow_mut();
            if let Some(children) = node.children.as_mut() {
                for (index_in_parent, index_in_tree) in children.iter().enumerate() {
                    let mut node = self.elements[*index_in_tree].as_ref().unwrap().borrow_mut();
                    f(&mut node.value, *index_in_tree, index_in_parent);
                }
            }
            Ok(())
        } else {
            return Err(TreeError::NodeDoesNotExist);
        }
    }

    pub fn dump_callback(&mut self, f: impl Fn(DumpCallbackType, Option<&T>) -> String + 'static) {
        self.dcb = Some(Box::new(f));
    }

    pub fn dump_structure(&self) {
        let head = match self.dcb {
            Some(ref f) => f(DumpCallbackType::Head, None),
            None => "Tree structure:".to_owned(),
        };
        println!("{}> {}", self.uid, head);
        if self.elements.len() == 0 || self.elements[0].is_none() {
            println!("{}> <Empty tree>", self.uid);
            return;
        }
        self.elements[0]
            .as_ref()
            .unwrap()
            .borrow()
            .dump_structure(self, 1);
    }
}

#[cfg(test)]
mod tests {

    use assert2::{check, let_assert};
    use rand::Rng;

    use super::*;

    #[derive(Debug)]
    struct Node {
        value: i32,
    }

    #[test]
    fn initialize() {
        let tree: Tree<Node> = Tree::new();
        check!(tree.capacity() == 0);
        check!(tree.len() == 0);
        tree.dump_structure();
    }

    #[test]
    fn add_node() {
        let mut tree: Tree<Node> = Tree::new();
        let rootidx = tree.add_node(TreeNodeType::Root, || Node { value: 1 });
        check!(let Ok(_) = rootidx);
        check!(tree.capacity() == 1);
        check!(rootidx.unwrap() == 0);
        check!(let Err(_) = tree
            .add_node(TreeNodeType::Root, || Node { value: 0 })
            );

        tree.get_node(0, |node, parent_index_in_tree| {
            check!(node.value == 1);
            check!( let None = parent_index_in_tree);
            Ok(())
        });

        check!(tree.len() == 1);
        tree.dump_structure();
    }

    #[test]
    fn get_node() {
        let mut tree: Tree<Node> = Tree::new();
        let rootidx = tree.add_node(TreeNodeType::Root, || Node { value: 1 });
        check!(let Ok(_) = rootidx);
        check!(tree.capacity() == 1);
        check!(rootidx.unwrap() == 0);
        check!(tree.len() == 1);
        check!(tree.get_node_mut(0).unwrap().value == 1);

        tree.get_node_mut(0).unwrap().value = 3;

        check!(tree.get_node_mut(0).unwrap().value == 3);

        tree.dump_structure();
        //
    }

    #[test]
    fn manipulate_node() {
        let mut tree: Tree<Node> = Tree::new();
        tree.add_node(TreeNodeType::Root, || Node { value: 100 });
        check!(tree.capacity() == 1);

        tree.get_node(0, |node, _| {
            node.value = 200;
            Ok(())
        });
        let mut lastvalue: i32 = 0;
        tree.get_node(0, |node, _| {
            lastvalue = node.value;
            Ok(())
        });
        check!(lastvalue == 200);

        tree.get_node_mut(0).unwrap().value = 123;

        tree.get_node(0, |node, _| {
            lastvalue = node.value;
            Ok(())
        });
        check!(lastvalue == 123);

        tree.dump_structure();
    }

    #[test]
    fn add_subchild() {
        let mut tree: Tree<Node> = Tree::new();
        tree.add_node(TreeNodeType::Root, || Node { value: 100 });

        let mut child = tree.add_node(TreeNodeType::Child(0), || Node { value: 200 });
        check!( let Ok(1) =  child );

        child = tree.add_node(TreeNodeType::Child(child.unwrap()), || Node { value: 200 });
        check!( let Ok(2) =  child );

        let lastchildindex = child.unwrap();

        tree.get_node(lastchildindex, |node, parent_index_in_tree| {
            check!(node.value == 200);
            assert_eq!(
                parent_index_in_tree,
                Some(1),
                "Parent index in tree should be 1"
            );
            Ok(())
        });

        if let Ok(info) = tree.get_node_info(lastchildindex) {
            check!(info.0 == 2); // index in tree
            check!(info.1 == Some(0)); // index in parent
            check!(info.2 == Some(1)); // parent index in tree
        };

        tree.dump_structure();
    }

    #[test]
    fn remove_root() {
        let mut tree: Tree<Node> = Tree::new();
        tree.add_node(TreeNodeType::Root, || Node { value: 100 });

        // Remove root
        check!(let Ok(_) = tree.remove_node(0));

        tree.dump_structure();
    }

    #[test]
    fn add_remove_children() {
        let mut tree: Tree<Node> = Tree::new();
        tree.add_node(TreeNodeType::Root, || Node { value: 1 });

        let mut child = tree
            .add_node(TreeNodeType::Child(0), || Node { value: 2 })
            .unwrap();
        check!( let Some(0) = tree.get_parent_index(child));

        child = tree
            .add_node(TreeNodeType::Child(0), || Node { value: 3 })
            .unwrap();
        check!( let Some(0) = tree.get_parent_index(child));

        child = tree
            .add_node(TreeNodeType::Child(0), || Node { value: 4 })
            .unwrap();
        check!( let Some(0) = tree.get_parent_index(child));

        check!(child == 3);

        //tree.dump_structure();
        check!(let Ok(_) = tree.remove_node(2));

        check!(tree.len() == 3);
        check!(tree.capacity() == 4);

        check!(let Ok(_) = tree.remove_node(3));

        child = tree
            .add_node(TreeNodeType::Child(1), || Node { value: 5 })
            .unwrap();
        check!(let Some(1)= tree.get_parent_index(child));

        tree.dump_structure();
    }
    #[test]
    fn remove_children_recourively() {
        let mut tree: Tree<Node> = Tree::new();
        tree.add_node(TreeNodeType::Root, || Node { value: 1 });

        let mut child = tree
            .add_node(TreeNodeType::Child(0), || Node { value: 2 })
            .unwrap();
        child = tree
            .add_node(TreeNodeType::Child(child), || Node { value: 3 })
            .unwrap();
        child = tree
            .add_node(TreeNodeType::Child(child), || Node { value: 4 })
            .unwrap();
        tree.dump_structure();

        let mut removed = tree.remove_node_and_children(1);

        check!(let Ok(_) = removed);
        check!(removed.unwrap().len() == 3);

        check!(tree.len() == 1);
        check!(tree.capacity() == 1);

        check!(let Ok(_) = tree.remove_node_and_children(0));

        check!(tree.len() == 0);
        check!(tree.capacity() == 0);

        tree.dump_structure();
    }
    #[test]
    fn foreach_children() {
        let mut tree: Tree<Node> = Tree::new();
        let rootidx = tree.add_node(TreeNodeType::Root, || Node { value: 1 });
        check!(let Ok(_) = rootidx);
        let rootidx = rootidx.unwrap();

        // Now add 3 children
        for n in 1..=5 {
            check!(let Ok(_) = tree
            .add_node(TreeNodeType::Child(rootidx), || Node { value: n }));
        }

        let mut cycle = 1;
        tree.foreach_children(rootidx, |node, index_in_tree, index_in_parent| {
            check!(node.value == cycle);
            check!(index_in_tree == cycle as usize);
            check!(index_in_parent == cycle as usize - 1usize);

            cycle += 1;
        });

        check!( let Ok(_) = tree.remove_node(3));

        cycle = 1;
        tree.foreach_children(rootidx, |node, index_in_tree, index_in_parent| {
            match cycle {
                1 => {
                    check!(node.value == 1);
                    check!(index_in_tree == 1);
                    check!(index_in_parent == 0);
                }
                2 => {
                    check!(node.value == 2);
                    check!(index_in_tree == 2);
                    check!(index_in_parent == 1);
                }
                3 => {
                    check!(node.value == 5);
                    check!(index_in_tree == 5);
                    check!(index_in_parent == 2);
                }
                4 => {
                    check!(node.value == 4);
                    check!(index_in_tree == 4);
                    check!(index_in_parent == 3);
                }
                _ => panic!("Should not happen"),
            }

            cycle += 1;
        });

        tree.dump_structure();        
    }

    #[test]
    fn massive_workload() {
        let mut rng = rand::thread_rng();
        let mut tree: Tree<Node> = Tree::new();
        let _dcb = |_type: DumpCallbackType, node: Option<&Node>| match _type {
            DumpCallbackType::Head => return "CTree structure:".to_owned(),
            DumpCallbackType::Node(index_in_tree, Some(parent_index_in_tree)) => {
                return format!(
                    "CNode[{}<^{}={}]",
                    parent_index_in_tree,
                    index_in_tree,
                    node.unwrap().value
                )
            }
            DumpCallbackType::Node(index_in_tree, None) => {
                return format!("CNode[§<^{}={}]", index_in_tree, node.unwrap().value)
            }
        };

        tree.dump_callback(_dcb);
        check!(let Ok(_) = tree.add_node(TreeNodeType::Root, || Node { value: 1 }));

        let mut child = 0;
        for i in 0..1000 {
            let mut rnd = if tree.capacity() == 1 {
                0
            } else {
                rng.gen_range(0..tree.capacity() - 1)
            };
            if tree.exists_node_at(rnd) {
                child = tree
                    .add_node(TreeNodeType::Child(rnd), || Node { value: i })
                    .unwrap();
            }
            if rng.gen_range(0..100) < 30 && tree.len() > 10 {
                rnd = if tree.capacity() <= 1 {
                    0
                } else {
                    rng.gen_range(0..tree.capacity() - 1)
                };
                if tree.exists_node_at(rnd) && tree.get_node_ref(rnd, |f| Ok(f.is_empty())).unwrap()
                {
                    check!(let Ok(_) = tree.remove_node(rnd));
                }
            }
        }

        tree.dump_structure();
    }
}
