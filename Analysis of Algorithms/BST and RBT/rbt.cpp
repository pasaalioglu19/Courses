// EB
// Implementing Red-Black Tree in C++

#include <iostream>

namespace RBT {
  struct Node {
    int data;
    std::string name;
    Node *parent;
    Node *left;
    Node *right;
    int color;
  };
}

class RedBlackTree {
private:
  RBT::Node* root; // The root of the tree is defined
  RBT::Node* TNULL; // Custom node created to represent empty nodes

  // Function to perform left rotation
  void leftRotate(RBT::Node* x) {
    RBT::Node* y = x->right;
    x->right = y->left;

    if (y->left != TNULL)
      y->left->parent = x;

    y->parent = x->parent;

    if (x->parent == NULL)
      root = y;
    else if (x == x->parent->left)
      x->parent->left = y;
    else
      x->parent->right = y;

    y->left = x;
    x->parent = y;
  }

  // Function to perform right rotation
  void rightRotate(RBT::Node* y) {
    RBT::Node* x = y->left;
    y->left = x->right;

    if (x->right != TNULL)
      x->right->parent = y;

    x->parent = y->parent;

    if (y->parent == NULL)
      root = x;
    else if (y == y->parent->left)
      y->parent->left = x;
    else
      y->parent->right = x;

    x->right = y;
    y->parent = x;
  }

  // Function to fix violations after inserting a node
  void insertFix(RBT::Node* z) {
    while (z->parent != NULL && z->parent->color == 1) {
      if (z->parent == z->parent->parent->left) {
        RBT::Node* y = z->parent->parent->right;
        if (y != NULL && y->color == 1) {
          z->parent->color = 0;
          y->color = 0;
          z->parent->parent->color = 1;
          z = z->parent->parent;
        } 
        else {
          if (z == z->parent->right) {
            z = z->parent;
            leftRotate(z);
          }
          z->parent->color = 0;
          z->parent->parent->color = 1;
          rightRotate(z->parent->parent);
        }
      } 
      else {
        RBT::Node* y = z->parent->parent->left;
        if (y != NULL && y->color == 1) {
          z->parent->color = 0;
          y->color = 0;
          z->parent->parent->color = 1;
          z = z->parent->parent;
        } 
        else {
          if (z == z->parent->left) {
            z = z->parent;
            rightRotate(z);
          }
          z->parent->color = 0;
          z->parent->parent->color = 1;
          leftRotate(z->parent->parent);
        }
      }
    }
    root->color = 0;
  }

  void preorder_2(RBT::Node* temp, std::pair<std::string, int> data[], int& dataIndex) {
    if (temp != TNULL) {
      // Insert the data into the array
      data[dataIndex++] = std::make_pair(temp->name, temp->data);

      // Traverse the left subtree
      preorder_2(temp->left, data, dataIndex);

      // Traverse the right subtree
      preorder_2(temp->right, data, dataIndex);
    }
  }

  void inorder_2(RBT::Node* temp, std::pair<std::string, int> data[], int& dataIndex) {
    if (temp != TNULL) {
      // Traverse the left subtree
      inorder_2(temp->left, data, dataIndex);

      // Insert the data into the array
      data[dataIndex++] = std::make_pair(temp->name, temp->data);

      // Traverse the right subtree
      inorder_2(temp->right, data, dataIndex);
    }
  }

  void postorder_2(RBT::Node* temp, std::pair<std::string, int> data[], int& dataIndex) {
    if (temp != TNULL) {
      // Traverse the left subtree
      postorder_2(temp->left, data, dataIndex);

      // Traverse the right subtree
      postorder_2(temp->right, data, dataIndex);

      // Insert the data into the array
      data[dataIndex++] = std::make_pair(temp->name, temp->data);
    }
  }

  // Helper function to transplant a subtree with another subtree
  void rbTransplant(RBT::Node* u, RBT::Node* v) {
    if (u->parent == NULL)
      root = v;
    else if (u == u->parent->left)
      u->parent->left = v;
    else
      u->parent->right = v;

    if (v != NULL)
      v->parent = u->parent;
  }

  // Function that deletes the node containing the desired population from the tree
  void deleteNode_2(RBT::Node* node_p, int test_pop){
    RBT::Node* z = TNULL;
    RBT::Node* x;
    RBT::Node* y;

    while (node_p != TNULL){
      if (node_p->data == test_pop)
      {
        z = node_p;
      }

      if (node_p->data <= test_pop)
      {
        node_p = node_p->right;
      }
      else{
        node_p = node_p->left;
      }
    }

    if (z == TNULL)
    {
      std::cout << "Test population not found in the tree" << std::endl;
      return;
    }
    
    y = z;
    int y_original_color = y->color;
    if (z->left == TNULL){
      x = z->right;
      rbTransplant(z, z->right);
    }
    else if (z->right == TNULL){
      x = z->left;
      rbTransplant(z, z->left);
    }
    else{
      y = getMinimum_2(z->right);
      y_original_color = y->color;
      x = y->right;
    
      if (y->parent == z){
        x->parent = y;
      }
      else{
        rbTransplant(y,y->right);
        y->right = z->right;
        y->right->parent = y;
      }

      rbTransplant(z,y);
      y->left = z->left;
      y->left->parent = y;
      y->color = z->color;
    }
    delete z;
    if (y_original_color == 0){
      deleteFix(x);
    }
  }

  // Helper function to fix Red-Black Tree violations after deletion
  void deleteFix(RBT::Node* x) {
    RBT::Node* s;
    while (x != root &&  x->color == 0) {
      if (x == x->parent->left) {
        s = x->parent->right;
        if (s->color == 1) {
          s->color = 0;
          x->parent->color = 1;
          leftRotate(x->parent);
          s = x->parent->right;
        }

        if (s->left->color == 0 && s->right->color == 0) {
          s->color = 1;
          x = x->parent;
        } 
        else {
          if (s->right->color == 0) {
            s->left->color = 0;
            s->color = 1;
            rightRotate(s);
            s = x->parent->right;
          }

          s->color = x->parent->color;
          x->parent->color = 0;
          s->right->color = 0;
          leftRotate(x->parent);
          x = root; 
        }
      } 
      else {
        s = x->parent->left;

        if (s->color == 1) {
          s->color = 0;
          x->parent->color = 1;
          rightRotate(x->parent);
          s = x->parent->left;
        }

        if (s->right->color == 0 && s->left->color == 0) {
          s->color = 1;
          x = x->parent;
        } 
        else {
          if (s->left->color == 0) {
              s->right->color = 0;
              s->color = 1;
              leftRotate(s);
              s = x->parent->left;
          }

          s->color = x->parent->color;
          x->parent->color = 0;
          s->left->color = 0;
          rightRotate(x->parent);
          x = root; 
        }
      }
    }

    if (x != NULL)
      x->color = 0;
  }

  RBT::Node* searchTree_2(RBT::Node* temp, int testpop) {
    if (temp == TNULL || testpop == temp->data){
      return temp;
    }
    if (testpop < temp->data){
      return searchTree_2(temp->left,testpop); // Traverse the left subtree
    }
    return searchTree_2(temp->right,testpop); // Traverse the right subtree
  }

  int getHeight_2(RBT::Node* temp){
    if (temp == TNULL){
      return 0;
    }
    else{
      int leftD = getHeight_2(temp->left); // Traverse the left subtree
      int rightD = getHeight_2(temp->right); // Traverse the right subtree

      // Calculates the longest branch
      if (leftD > rightD)
        return (leftD + 1); //If its blackheight just change '1' with (temp->color == BLACK ? 1 : 0)
      else
        return (rightD + 1); //If its blackheight just change '1' with (temp->color == BLACK ? 1 : 0)
    }
  }

  RBT::Node* getMaximum_2(RBT::Node* temp){
    // Returns the node with the maximum value by going to the far right
    while (temp->right != TNULL) {
      temp = temp->right;
    }
    return temp;
  }

  RBT::Node* getMinimum_2(RBT::Node* temp) {
    // Returns the node with the maximum value by going to the far left
    while (temp->left != TNULL) {
      temp = temp->left;
    }
    return temp;
  }

  int getTotalNodes_2(RBT::Node* temp) {
    if (temp == TNULL) {
      return 0;
    } 
    else {
      return 1 + getTotalNodes_2(temp->left) + getTotalNodes_2(temp->right); // Adds 1 for each non-empty node traversed
    }
  }

public:
  // Sets the initial state of red black tree
  RedBlackTree() {
    TNULL = new RBT::Node;
    TNULL->color = 0;
    TNULL->left = NULL;
    TNULL->right = NULL;
    root = TNULL;
  }

  void preorder(std::pair<std::string, int> data[], int dataIndex) {
    preorder_2(root, data, dataIndex); // Calls the preorder function by sending root as a parameter
  }
 
  void inorder(std::pair<std::string, int> data[], int dataIndex) {
    inorder_2(root, data, dataIndex); // Calls the inorder function by sending root as a parameter
  }

  void postorder(std::pair<std::string, int> data[], int dataIndex) {
    postorder_2(root, data, dataIndex); // Calls the postorder function by sending root as a parameter
  }

  RBT::Node* searchTree(int testpop) {
    return searchTree_2(root,testpop); // Calls the searchTree function by sending root as a parameter
  }

  RBT::Node* successor(RBT::Node* temp) {
    // If the node has a right subtree, its find the leftmost node in that subtree
    if (temp->right != TNULL) {
      return getMinimum_2(temp->right);
    }

    // If the node does not have a right subtree, it finds the node right behind
    RBT::Node* y = temp->parent;
    while (y != TNULL && temp == y->right)
    {
      temp = y;
      y = y->parent;
    }
    return y;
  }

  RBT::Node* predecessor(RBT::Node* temp) {
    // If the node has a left subtree, its find the rightmost node in that subtree
    if (temp->left != TNULL) {
      return getMaximum_2(temp->left);
    }

    // If the node does not have a left subtree, it finds the node right behind
    RBT::Node* y = temp->parent;
    while (y != TNULL && temp == y->left)
    {
      temp = y;
      y = y->parent;
    }
    
    return y;
  }

  void insert(std::string city, int population) { 
    // Initialize to z
    RBT::Node* z = new RBT::Node; 
    z->parent = NULL;
    z->data = population;
    z->name = city;
    z->left = TNULL;
    z->right = TNULL;
    z->color = 1; 

    RBT::Node* y = NULL;
    RBT::Node* x = this->root;

    while (x != TNULL) {
      y = x;
      if (z->data < x->data)
        x = x->left;
      else
        x = x->right;
    }

    z->parent = y;

    if (y == NULL)
      root = z;
    else if (z->data < y->data)
      y->left = z;
    else
      y->right = z;

    // Fix to Red-Black Tree violations
    insertFix(z);
  }

  void deleteNode(int test_pop) {
    deleteNode_2(this->root,test_pop); // Calls the deleteNode function by sending root as a parameter
  }

  int getHeight() {
    return getHeight_2(root) - 1; //This mines 1 arranged by sample log_pop1.txt
  }

  RBT::Node* getMaximum() {
    return getMaximum_2(root); // Calls the getMaximum function by sending root as a parameter
  }

  RBT::Node* getMinimum() {
    return getMinimum_2(root); // Calls the getMinimum function by sending root as a parameter
  }

  int getTotalNodes() {
    return getTotalNodes_2(root); // Calls the getTotalNodes function by sending root as a parameter
  }

};