//
//  suffix-tree.h
//  suffix-tree
//
//  Created by Brett Wines on 1/5/13.
//  Copyright (c) 2013 Brett Wines. All rights reserved.
//

#ifndef __suffix_tree__suffix_tree__
#define __suffix_tree__suffix_tree__

#include <iostream>
#include <vector>

using namespace std;

class suffix_tree {
public:
    suffix_tree(string str);
    ~suffix_tree();
    
    void print_tree();
    
    void append_suffix_str(string new_suffix);
    void append_suffix_char(char new_suffix);
private:
    //structs
    struct node; //forward declaration
    struct edge {
        node* src;
        node* dest;
        
        //to be removed in final implementation
        string stored_string;
        
        //indices into string
        size_t start;
        int end; //-1 if to end of string
        
        edge();
        
        edge(node* src, node* dest, size_t start, int end);
        
        string get_string();
        
    };
    
    struct node {
        char ch;
        
        //to be removed in final implementation
        string stored_string;
        
        node* suffix_link;
        vector<edge> edges;
        
        node();
        
        void add_child(char ch);
        
        bool contains_edge_starting_with_char(char ch, int index);
        
        string get_string();

    };

    struct active_point {
        node* active_node;
        char active_edge;
        size_t active_length;
    };
    
    //methods
    void delete_tree(node* curr);

    string get_stored_string();
    
    void create_leaf(char ch);
    void split_internal_node(char ch, node*& last_split_node);

    void print_tree_wrapped(node* curr, string so_far);

    bool should_split_internal_node(char new_suffix);
    
    //ivars
    string stored_string;
    
    node* root;
    
    active_point active_point;
    size_t remainder;
};

#endif /* defined(__suffix_tree__suffix_tree__) */
