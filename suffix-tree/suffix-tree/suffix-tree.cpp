//
//  suffix-tree.cpp
//  suffix-tree
//
//  Created by Brett Wines on 1/5/13.
//  Copyright (c) 2013 Brett Wines. All rights reserved.
//

#include "suffix-tree.h"

suffix_tree::suffix_tree(string str) {
    root = NULL;
    
    active_point.active_node = root;
    active_point.active_edge = '$';
    active_point.active_length = 0;
    
    str += "$"; //terminating char
    append_suffix_str(str);
}

suffix_tree::~suffix_tree() {
    delete_tree(root);
}

void suffix_tree::delete_tree(node* curr) {
    if (NULL == curr)
        return;
    
    for (int i=0; i<curr->edges.size(); i++) {
        delete_tree(curr->edges[i].dest);
    }
    
    delete curr;
}

void suffix_tree::append_suffix_str(string new_suffix) {
    for (int i=0; i<new_suffix.length(); i++) {
        append_suffix_char(new_suffix[i]);
    }
}

void suffix_tree::create_leaf(char ch) {
    active_point.active_node->add_child(ch);
}

void suffix_tree::node::add_child(char ch) {
    node* child = new node();
    
    string str = string(1, ch); //convert char to string
    size_t length = get_string().length();
    edge edge_to_child(this, child, length-1, (int)length);
    this->edges.push_back(edge_to_child);
}

void suffix_tree::split_internal_node(char ch, node*& last_split_node) {
    node* split_node = NULL;
    
    split_node->suffix_link = last_split_node;
    
    last_split_node = split_node;
}

bool suffix_tree::should_split_internal_node(char new_suffix) {
    if (true) {//if (active_point.active_node->contains_edge_starting_with_char(new_suffix, active_point.active_edgeaasntehu)) {
        return true;
    } else {
        return false;
    }
}

// Ukkonen's algorithm
void suffix_tree::append_suffix_char(char new_suffix) {
    stored_string += new_suffix;
    
    node* last_split_node = NULL;
    remainder++;
    while (true) { //until something...
        if (should_split_internal_node(new_suffix)) {
            split_internal_node(new_suffix, last_split_node);
        }
    }
}

//util

string suffix_tree::get_stored_string() {
    return stored_string;
}

void suffix_tree::print_tree() {
    print_tree_wrapped(root, "");
}

void suffix_tree::print_tree_wrapped(node* curr, string so_far) {
    //leaf node
    if (curr->edges.size() == 0) {
        cout << so_far << endl;
        return;
    }
    
    //internal node
    for (auto edge=curr->edges.begin(); edge!=curr->edges.end(); edge++) {
        string edge_label = edge->get_string();
        //mentally expand to suffix trie
        for (int i=edge_label.length()-1; i>=0; i--) {
            string so_far_updated = so_far + edge_label.substr(i);
            
            print_tree_wrapped(edge->dest, so_far_updated);
        }
    }
}


/*node.h below*/
suffix_tree::node::node()
: suffix_link(NULL)
{}

string suffix_tree::node::get_string() {
    return this->stored_string;
}

bool suffix_tree::node::contains_edge_starting_with_char(char ch, int index) {
    for (auto edge=this->edges.begin(); edge!=this->edges.end(); edge++) {
        if (edge->get_string()[index] == ch) {
            return true;
        }
    }
}

/*edge.h below*/
suffix_tree::edge::edge()
: src(NULL)
, dest(NULL)
{}

suffix_tree::edge::edge(node* src, node* dest, size_t start, int end)
: start(start)
, end(end)
, src(src)
, dest(dest)
{}

string suffix_tree::edge::get_string() {
    return this->stored_string;
}
