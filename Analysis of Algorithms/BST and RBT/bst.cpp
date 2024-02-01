// EB
// Implementing Binary Search Tree in C++

#include <iostream>

namespace BST {
  struct Node {
    int data;
    std::string name;
    Node *parent;
    Node *left;
    Node *right;
  };
}

class BinarySearchTree {
private:
  BST::Node* root; // The root of the tree is defined

  //Create new Node
  BST::Node* newNode(BST::Node* parent, std::string city, int population){
      BST::Node* new_node = new BST::Node;
      new_node->parent = parent;
      new_node->data = population;
      new_node->name = city;
      new_node->left = NULL;
      new_node->right = NULL;
      return new_node;
  } 

  void preorder_2(BST::Node* temp, std::pair<std::string, int> data[], int& dataIndex) {
    if (temp) {
      // Insert the data into the array
      data[dataIndex++] = std::make_pair(temp->name, temp->data);

      // Traverse the left subtree
      preorder_2(temp->left, data, dataIndex);

      // Traverse the right subtree
      preorder_2(temp->right, data, dataIndex);
    }
  }

  void inorder_2(BST::Node* temp, std::pair<std::string, int> data[], int& dataIndex) {
    if (temp) {
      // Traverse the left subtree
      inorder_2(temp->left, data, dataIndex);

      // Insert the data into the array
      data[dataIndex++] = std::make_pair(temp->name, temp->data);

      // Traverse the right subtree
      inorder_2(temp->right, data, dataIndex);
    }
  }

  void postorder_2(BST::Node* temp, std::pair<std::string, int> data[], int& dataIndex) {
    if (temp) {
      // Traverse the left subtree
      postorder_2(temp->left, data, dataIndex);

      // Traverse the right subtree
      postorder_2(temp->right, data, dataIndex);

      // Insert the data into the array
      data[dataIndex++] = std::make_pair(temp->name, temp->data);
    }
  }

  BST::Node* insert_2(BST::Node* temp, BST::Node* parent, std::string city, int population){
    //If temp is NULL, thats mean new node can be created
    if (temp == NULL)
    {
      return newNode(parent, city, population);
    }

    //Traverse left subtree recursively
    if (population < temp->data){
      temp->left = insert_2(temp->left,temp,city,population);
    }
    //Traverse right subtree recursively
    else{
      temp->right = insert_2(temp->right,temp,city,population);
    }

    return temp;
  }

  void deleteNode_2(BST::Node* node_p,int test_pop) {
    if (node_p == NULL)
      return;

    // If it has neither left nor right child, it will directly remove that node
    if (node_p->left == NULL && node_p->right == NULL){
      delete node_p;
      return;
    }

    // If there is a right child, use the successor function to replace the node to be deleted with the successor and eliminate it
    if (node_p->right != NULL){
      BST::Node*temp = successor(node_p);
      node_p->data = temp->data;
      node_p->name = temp->name;
      if (temp->right != NULL) //If deleteToNode has a right child, this right child is linked to its parent
      {
        node_p->right = temp->right;
        temp->right->parent = node_p;
      }
      else {
        if (temp->parent->left == temp)
          temp->parent->left = NULL;
        else
          temp->parent->right = NULL;
      }
      delete temp;
      return;
    }

    //all below is has left child but has not right child condition
    if (node_p->parent == NULL) //so its head
    { 
      root = node_p->left;
      root->parent = NULL;
    }
    else if (node_p->parent->data >= node_p->data){
      node_p->parent->left = node_p->left;
      node_p->left->parent = node_p->parent;
    }

    else{
      node_p->parent->right = node_p->left;
      node_p->left->parent = node_p->parent;
    }

    delete node_p;
    return;    
  }

  BST::Node* searchTree_2(BST::Node* temp, int test_pop) {
    if (!temp){
      return NULL;
    }

    if (test_pop == temp->data){
      return temp;
    }
    
    else if (test_pop < temp->data){
      return searchTree_2(temp->left,test_pop); // Traverse the left subtree
    }
    else{
      return searchTree_2(temp->right,test_pop); // Traverse the right subtree
    }
  }

  int getHeight_2(BST::Node* temp){
    if (temp == NULL)
    {
      return 0;
    }
    else{
      int leftD = getHeight_2(temp->left); // Traverse the left subtree
      int rightD = getHeight_2(temp->right); // Traverse the right subtree

      // Calculates the longest branch
      if (leftD > rightD)
        return (leftD + 1);
      else
        return (rightD + 1);
    }
  }

  BST::Node* getMaximum_2(BST::Node* temp){
    // Returns the node with the maximum value by going to the far right
    while (temp->right != NULL) {
      temp = temp->right;
    }
    return temp;
  }

  BST::Node* getMinimum_2(BST::Node* temp) {
    // Returns the node with the maximum value by going to the far left
    while (temp->left != NULL) {
      temp = temp->left;
    }
    return temp;
  }

  int getTotalNodes_2(BST::Node* temp) {
    if (temp == NULL) {
      return 0;
    } 
    else {
      return 1 + getTotalNodes_2(temp->left) + getTotalNodes_2(temp->right); // Adds 1 for each non-empty node traversed
    }
  }

public:
  BinarySearchTree() {
    root = NULL; // Root initialized null
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

  BST::Node* searchTree(int test_pop) {
    return searchTree_2(root,test_pop); // Calls the searchTree function by sending root as a parameter
  }

  BST::Node* successor(BST::Node* temp) {
    // If the node has a right subtree, its find the leftmost node in that subtree
    if (temp->right != NULL) {
      return getMinimum_2(temp->right);
    }

    // If the node does not have a right subtree, it finds the node right behind
    BST::Node* y = temp->parent;
    while (y != NULL && temp == y->right)
    {
      temp = y;
      y = y->parent;
    }

    return y;
  }

  BST::Node* predecessor(BST::Node* temp) {
    // If the node has a left subtree, its find the rightmost node in that subtree
    if (temp->left != NULL) {
      return getMaximum_2(temp->left);
    }

    // If the node does not have a left subtree, it finds the node right behind
    BST::Node* y = temp->parent;
    while (y != NULL && temp == y->left)
    {
      temp = y;
      y = y->parent;
    }
    return y;
}

  void insert(std::string city, int population) {
    this->root = insert_2(root,NULL,city,population); // Calls the insert function by sending root as a parameter
  }

  void deleteNode(int test_pop) {
    BST::Node* deletionNode = searchTree(test_pop); // It finds the Node of testPopulation 
    deleteNode_2(deletionNode,test_pop); // Calls the deleteNode function by sending root as a parameter
  }

  int getHeight() {
    return getHeight_2(root) - 1; //This mines 1 arranged by sample log_pop1.txt
  }

  BST::Node* getMaximum() {
    return getMaximum_2(root); // Calls the getMaximum function by sending root as a parameter
  }

  BST::Node* getMinimum() {
    return getMinimum_2(root); // Calls the getMinimum function by sending root as a parameter
  }
  
  int getTotalNodes() {
    return getTotalNodes_2(root); // Calls the getTotalNodes function by sending root as a parameter
  }

};