# AVL-Bäume für CLISP
# Bruno Haible 1993-1999

# Ziel: Eine Menge von Elementen sortiert zu halten, in der ab und zu
# einmal ein Element dazukommt oder ein Element seinen Sortierschlüssel
# verändert.

# ==============================================================================
# Spezifikation:

# Von außen ist einzustellen:
# Identifier AVLID :
#   Identifier, der die Inkarnation dieser Package identifiziert
# Typ AVL_ELEMENT :
#   Typ der Elemente, die in einem AVL-Baum eingetragen werden.
# Funktion AVL_EQUAL, mit
#   local boolean AVL_EQUAL (AVL_ELEMENT element1, AVL_ELEMENT element2);
#   stellt fest, ob zwei Elemente als gleich gelten.
#   In einem AVL-Baum dürfen keine zwei Elemente abgespeichert werden, die
#   als gleich gelten. (D.h. kein Element darf doppelt abgespeichert werden.)
# Typ AVL_KEY :
#   Typ des Key, nach dem ein AVL-Baum sortiert wird.
# Funktion AVL_KEYOF, mit
#   local AVL_KEY AVL_KEYOF (AVL_ELEMENT element);
#   liefert den Sortier-Key eines Elements, das in einem AVL-Baum sitzt.
# Typ AVL_SIGNED_INT :
#   Vorzeichenbehafteter Integer-Typ, breit genug für AVL_COMPARE-Werte.
#   (Normalerweise `sintL'. `signean' ist zu kurz.)
# Funktion AVL_COMPARE, mit
#   local AVL_SIGNED_INT AVL_COMPARE (AVL_KEY key1, AVL_KEY key2);
#   liefert >0 falls key1>key2, <0 falls key1<key2, 0 falls key1=key2.
# NB: Aus AVL_EQUAL(element1,element2) sollte
#     AVL_COMPARE(KEY_OF(element1),KEY_OF(element2)) = 0 folgen,
#     sonst funktionieren avl_member und avl_delete nicht!
# Dann ist avl.c zu includen.
# Dann kann ein eigener struct-Typ NODE definiert werden:
#   typedef struct NODE { ...; NODEDATA nodedata; ...; } NODE;
#   #define HAVE_NODE  # nur zum Anzeigen, dass NODE definiert wurde
# Dann ist avl.c abermals zu includen.
# Werden einige der Macros NO_AVL_MEMBER, NO_AVL_INSERT[1], NO_AVL_DELETE[1],
# NO_AVL_LEAST, NO_AVL_MOVE, NO_AVL_SORT definiert, so werden die
# entsprechenden Funktionen nicht definiert.

# ==============================================================================

#ifndef __AVL_D_
#define __AVL_D_

# Deklarations-Teil:

#ifndef ALLOC
  #ifndef NO_AVL_INSERT
    #define ALLOC(eltype,number)  ((eltype*) malloc((uintL)sizeof(eltype) * (uintL)(number)))
  #endif
  #ifndef NO_AVL_DELETE
    #define FREE(item)  free(item)
  #endif
#endif

#ifndef AVL
  # Eine Art "AVL-Package" für Identifier von Typen und Funktionen:
  #define AVL(incarnation,identifier)  CONCAT4(avl_,incarnation,_,identifier)
#endif

#define NODE        AVL(AVLID,node)
#define ELEMENT     AVL_ELEMENT
#define EQUAL       AVL_EQUAL
#define KEY         AVL_KEY
#define KEYOF       AVL_KEYOF
#define HEIGHT      uintBWL
#define MAXHEIGHT   41
#define SIGNED_INT  AVL_SIGNED_INT
#define COMPARE     AVL_COMPARE

#define NODEDATA  \
  struct { struct NODE * left;  # linker Teilbaum                               \
           struct NODE * right; # rechter Teilbaum                              \
           HEIGHT height;       # 1+max(heightof(left),heightof(right))         \
           ELEMENT value;       # an der Spitze dieses Baumes stehendes Element \
         }

#else

# ------------------------------------------------------------------------------
# Implementations-Teil:

#define TYPEDEF  typedef  # kleiner Trick, um TRADDECL zu überlisten

#ifndef HAVE_NODE
  typedef struct NODE { NODEDATA nodedata; } NODE;
#endif

# Ein AVL-Baum ist entweder leer oder ein NODE.
# Der leere Baum hat die Höhe 0, ein NODE hat als Höhe das Maximum der Höhen
# der beiden Teilbäume + 1.
  #define EMPTY  ((NODE *) 0)
  #define heightof(tree)  ((tree)==EMPTY ? 0 : (tree)->nodedata.height)

# Invarianten eines jeden AVL-Baumes:
# 1. Die Höhe eines jeden NODE ist korrekt berechnet:
#    node.height = 1+max(heightof(node.left),heightof(node.right))
# 2. Die Höhen der Teilbäume eines jeden NODE unterscheiden sich um höchstens 1:
#    | heightof(node.left) - heightof(node.right) | <= 1
# 3. In jedem NODE gilt:
#    forall x in node.left : COMPARE(KEYOF(x.value),KEYOF(node.value)) <= 0,
#    forall x in node.right : COMPARE(KEYOF(x.value),KEYOF(node.value)) >= 0.
# Ein AVL-Baum der Höhe h hat also mindestens F_(h+2) [Fibonacci-Zahl] und
# höchstens 2^h - 1 Elemente. Also h<=41 (denn ein Baum mit Höhe h>=42 hätte
# mindestens F_44 Elemente, und wegen sizeof(NODE) * F_44 > 2^32 passt das
# in keinen 32-Bit-Adressraum.) Daher reicht auch ein uintB für HEIGHT.

# Stellt fest, ob in einem Baum ein Element mit einem gegebenen Key vorkommt.
#ifndef NO_AVL_MEMBER0
  local NODE* AVL(AVLID,member0) (KEY key, NODE * tree);
  local NODE* AVL(AVLID,member0) (key,tree)
    var KEY key;
    var NODE * tree;
    { loop
        { if (tree == EMPTY) { return (NODE*)NULL; }
         {var SIGNED_INT sign = COMPARE(key,KEYOF(tree->nodedata.value));
          if (sign == 0) # gefunden?
            { return tree; }
          if (sign < 0)
            # key < KEYOF(tree->nodedata.value)  --> Suche im linken Teilbaum:
            { tree = tree->nodedata.left; }
            else
            # key > KEYOF(tree->nodedata.value)  --> Suche im rechten Teilbaum:
            { tree = tree->nodedata.right; }
    }   }}
#endif

# Stellt fest, ob in einem Baum ein Element vorkommt.
# Setzt voraus, dass keine zwei Elemente mit demselben Key im Baum vorkommen.
#ifndef NO_AVL_MEMBER
  local NODE* AVL(AVLID,member) (ELEMENT element, NODE * tree);
  local NODE* AVL(AVLID,member) (element,tree)
    var ELEMENT element;
    var NODE * tree;
    { var KEY key = KEYOF(element);
      loop
        { if (tree == EMPTY) { return (NODE*)NULL; }
         {var SIGNED_INT sign = COMPARE(key,KEYOF(tree->nodedata.value));
          if (sign == 0)
            { if (EQUAL(element,tree->nodedata.value)) # gefunden?
                { return tree; }
                else
                { return (NODE*)NULL; }
            }
          if (sign < 0)
            # key < KEYOF(tree->nodedata.value)  --> Suche im linken Teilbaum:
            { tree = tree->nodedata.left; }
            else
            # key > KEYOF(tree->nodedata.value)  --> Suche im rechten Teilbaum:
            { tree = tree->nodedata.right; }
    }   }}
#endif

# Stellt die Balance neu her: Beim Einfügen bzw. Löschen eines Elements
# eines Baumes ist eine Folge nodes[0],...,nodes[k-1] von Teilbäumen
# (mit nodes[i+1] = nodes[i] -> (left oder right) für alle i)
# neu auszubalancieren. Da dabei die Wurzel eines Teilbaums sich
# verändern kann, müssen alle nodes[i] nicht NODE*, sondern NODE** sein.
  local void AVL(AVLID,rebalance) (NODE** * nodeplaces_ptr, uintC count);
  local void AVL(AVLID,rebalance) (nodeplaces_ptr,count)
    var NODE** * nodeplaces_ptr;
    var uintC count;
    { dotimesC(count,count,
        { var NODE** nodeplace = *--nodeplaces_ptr;
          var NODE* node = *nodeplace; # nächster zu balancierender Teilbaum
          var NODE* nodeleft = node->nodedata.left;
          var NODE* noderight = node->nodedata.right;
          var HEIGHT heightleft = heightof(nodeleft);
          var HEIGHT heightright = heightof(noderight);
          if (heightright + 1 < heightleft)
            # Teilbaum linkslastig, rotiere von links nach rechts: #
            #                                                      #
            #                            *                         #
            #                          /   \                       #
            #                       n+2      n                     #
            #                                                      #
            { var NODE* nodeleftleft = nodeleft->nodedata.left;
              var NODE* nodeleftright = nodeleft->nodedata.right;
              var HEIGHT heightleftright = heightof(nodeleftright);
              if (heightof(nodeleftleft) >= heightleftright)
                #                                                        #
                #                *                    n+2|n+3            #
                #              /   \                  /    \             #
                #           n+2      n      -->      /   n+1|n+2         #
                #           / \                      |    /    \         #
                #         n+1 n|n+1                 n+1  n|n+1  n        #
                #                                                        #
                { node->nodedata.left = nodeleftright; nodeleft->nodedata.right = node;
                  nodeleft->nodedata.height = 1 + (node->nodedata.height = 1 + heightleftright);
                  *nodeplace = nodeleft;
                }
                else
                #                                                        #
                #                *                     n+2               #
                #              /   \                 /     \             #
                #           n+2      n      -->    n+1     n+1           #
                #           / \                    / \     / \           #
                #          n  n+1                 n   L   R   n          #
                #             / \                                        #
                #            L   R                                       #
                #                                                        #
                { nodeleft->nodedata.right = nodeleftright->nodedata.left;
                  node->nodedata.left = nodeleftright->nodedata.right;
                  nodeleftright->nodedata.left = nodeleft;
                  nodeleftright->nodedata.right = node;
                  nodeleft->nodedata.height = node->nodedata.height = heightleftright;
                  nodeleftright->nodedata.height = heightleft;
                  *nodeplace = nodeleftright;
                }
            }
          elif (heightleft + 1 < heightright)
            # Teilbaum rechtslastig, rotiere von rechts nach links:
            # (Analog zu oben, nur 'left' <--> 'right' vertauscht.)
            { var NODE* noderightright = noderight->nodedata.right;
              var NODE* noderightleft = noderight->nodedata.left;
              var HEIGHT heightrightleft = heightof(noderightleft);
              if (heightof(noderightright) >= heightrightleft)
                { node->nodedata.right = noderightleft; noderight->nodedata.left = node;
                  noderight->nodedata.height = 1 + (node->nodedata.height = 1 + heightrightleft);
                  *nodeplace = noderight;
                }
                else
                { noderight->nodedata.left = noderightleft->nodedata.right;
                  node->nodedata.right = noderightleft->nodedata.left;
                  noderightleft->nodedata.right = noderight;
                  noderightleft->nodedata.left = node;
                  noderight->nodedata.height = node->nodedata.height = heightrightleft;
                  noderightleft->nodedata.height = heightright;
                  *nodeplace = noderightleft;
                }
            }
          else
            { var HEIGHT height = # neue Gesamthöhe
                (heightleft<heightright ? heightright : heightleft) + 1;
              # Gesamthöhe dieses Teilbaumes bleibt unverändert ->
              # die diesen enthaltenden Teilbäume sind bereits ausbalanciert.
              if (height == node->nodedata.height) break;
              node->nodedata.height = height;
            }
        });
    }

# Fügt ein Element in einen AVL-Baum ein und liefert den neuen AVL-Baum.
#ifndef NO_AVL_INSERT
  local NODE* AVL(AVLID,insert) (ELEMENT value, NODE* tree);
  local NODE* AVL(AVLID,insert) (value,tree)
    var ELEMENT value;
    var NODE* tree;
    { var KEY key = KEYOF(value);
      var NODE** nodeplace = &tree;
      var NODE** stack[MAXHEIGHT]; # Ein kleiner Privat-Stack
      var uintC stack_count = 0; # Anzahl der Elemente auf dem Stack
      var NODE** * stack_ptr = &stack[0]; # stets = &stack[stack_count]
      loop
        { var NODE* node = *nodeplace;
          if (node == EMPTY) break;
          *stack_ptr++ = nodeplace; stack_count++;
          if (COMPARE(key,KEYOF(node->nodedata.value)) < 0)
            # key < KEYOF(node->nodedata.value)  --> im linken Teilbaum einfügen:
            { nodeplace = &node->nodedata.left; }
            else
            # key >= KEYOF(node->nodedata.value)  --> im rechten Teilbaum einfügen:
            { nodeplace = &node->nodedata.right; }
        }
     {var NODE* new_node = ALLOC(NODE,1);
      new_node->nodedata.left = EMPTY; new_node->nodedata.right = EMPTY; new_node->nodedata.height = 1;
      new_node->nodedata.value = value;
      *nodeplace = new_node;
      AVL(AVLID,rebalance)(stack_ptr,stack_count);
      return tree;
    }}
#endif
# Dito, jedoch ohne ALLOC aufzurufen:
#ifndef NO_AVL_INSERT1
  local NODE* AVL(AVLID,insert1) (NODE* new_node, NODE* tree);
  local NODE* AVL(AVLID,insert1) (new_node,tree)
    var NODE* new_node;
    var NODE* tree;
    { var KEY key = KEYOF(new_node->nodedata.value);
      var NODE** nodeplace = &tree;
      var NODE** stack[MAXHEIGHT]; # Ein kleiner Privat-Stack
      var uintC stack_count = 0; # Anzahl der Elemente auf dem Stack
      var NODE** * stack_ptr = &stack[0]; # stets = &stack[stack_count]
      loop
        { var NODE* node = *nodeplace;
          if (node == EMPTY) break;
          *stack_ptr++ = nodeplace; stack_count++;
          if (COMPARE(key,KEYOF(node->nodedata.value)) < 0)
            # key < KEYOF(node->nodedata.value)  --> im linken Teilbaum einfügen:
            { nodeplace = &node->nodedata.left; }
            else
            # key >= KEYOF(node->nodedata.value)  --> im rechten Teilbaum einfügen:
            { nodeplace = &node->nodedata.right; }
        }
      new_node->nodedata.left = EMPTY;
      new_node->nodedata.right = EMPTY;
      new_node->nodedata.height = 1;
      *nodeplace = new_node;
      AVL(AVLID,rebalance)(stack_ptr,stack_count);
      return tree;
    }
#endif

# Entfernt ein Element aus einem AVL-Baum und liefert den neuen AVL-Baum.
# Setzt voraus, dass keine zwei Elemente mit demselben Key im Baum vorkommen.
#ifndef NO_AVL_DELETE
  local NODE* AVL(AVLID,delete) (ELEMENT value, NODE* tree);
  local NODE* AVL(AVLID,delete) (value,tree)
    var ELEMENT value;
    var NODE* tree;
    { var KEY key = KEYOF(value);
      var NODE** nodeplace = &tree;
      var NODE** stack[MAXHEIGHT]; # Ein kleiner Privat-Stack
      var uintC stack_count = 0; # Anzahl der Elemente auf dem Stack
      var NODE** * stack_ptr = &stack[0]; # stets = &stack[stack_count]
      var NODE* node_to_delete;
      loop
        { var NODE* node = *nodeplace;
          if (node == EMPTY) goto fertig; # Element nicht gefunden
          *stack_ptr++ = nodeplace; stack_count++;
         {var SIGNED_INT sign = COMPARE(key,KEYOF(node->nodedata.value));
          if (sign == 0)
            { if (EQUAL(value,node->nodedata.value)) # gefunden?
                { node_to_delete = node; break; }
                else
                goto fertig;
            }
          if (sign < 0)
            # key < KEYOF(node->nodedata.value)  --> im linken Teilbaum entfernen:
            { nodeplace = &node->nodedata.left; }
            else
            # key > KEYOF(node->nodedata.value)  --> im rechten Teilbaum entfernen:
            { nodeplace = &node->nodedata.right; }
        }}
     {var NODE** nodeplace_to_delete = nodeplace;
      # node_to_delete = *nodeplace_to_delete ist zu entfernen.
      if (node_to_delete->nodedata.left == EMPTY)
        # node_to_delete wird ersetzt durch node_to_delete->nodedata.right.
        { *nodeplace_to_delete = node_to_delete->nodedata.right;
          stack_ptr--; stack_count--; # doch kein rebalance bei *nodeplace_to_delete!
        }
        else
        # node_to_delete wird ersetzt durch das am weitesten rechts gelegenene
        # Element von node_to_delete->nodedata.left.
        { var NODE** * stack_ptr_to_delete = stack_ptr;
          var NODE** nodeplace = &node_to_delete->nodedata.left;
          var NODE* node;
          loop
            { node = *nodeplace;
              if (node->nodedata.right == EMPTY) break;
              *stack_ptr++ = nodeplace; stack_count++;
              nodeplace = &node->nodedata.right;
            }
          *nodeplace = node->nodedata.left;
          # node nimmt die Stellung von node_to_delete ein:
          node->nodedata.left = node_to_delete->nodedata.left;
          node->nodedata.right = node_to_delete->nodedata.right;
          node->nodedata.height = node_to_delete->nodedata.height;
          *nodeplace_to_delete = node; # statt node_to_delete
          # Der Rebalance-Stack (Weg von der Wurzel nach unten) führt jetzt
          # nicht mehr über node_to_delete, sondern über node:
          *stack_ptr_to_delete = &node->nodedata.left; # statt &node_to_delete->nodedata.left
     }  }
      FREE(node_to_delete);
      AVL(AVLID,rebalance)(stack_ptr,stack_count);
      fertig:
      return tree;
    }
#endif

# Entfernt ein Element aus einem AVL-Baum und liefert den neuen AVL-Baum.
# Ohne FREE aufzurufen.
#ifndef NO_AVL_DELETE1
  local NODE* AVL(AVLID,delete1) (NODE* node_to_delete, NODE* tree);
  # Stellt fest, wo in einem Baum ein Element node_to_delete (mit Key key)
  # vorkommt. Legt den Pfad ab stack_ptr ab, und liefert das neue stack_ptr.
  local NODE** * AVL(AVLID,delete1find) (NODE* node_to_delete, KEY key, NODE* tree, NODE** * stack_ptr);
  local NODE** * AVL(AVLID,delete1find) (node_to_delete,key,tree,stack_ptr)
    var NODE* node_to_delete;
    var KEY key;
    var NODE* tree;
    var NODE** * stack_ptr;
    { loop
        { if (tree == EMPTY) { return (NODE***)NULL; }
         {var SIGNED_INT sign = COMPARE(key,KEYOF(tree->nodedata.value));
          if (sign == 0)
            # key = KEYOF(tree->nodedata.value)  --> Suche in beiden Teilbäumen:
            { if (tree == node_to_delete) { return stack_ptr; }
              *stack_ptr = &tree->nodedata.left;
             {var NODE*** part = AVL(AVLID,delete1find)(node_to_delete,key,tree->nodedata.left,stack_ptr+1);
              if (part) { return part; }
             }
              *stack_ptr = &tree->nodedata.right;
             {var NODE*** part = AVL(AVLID,delete1find)(node_to_delete,key,tree->nodedata.right,stack_ptr+1);
              if (part) { return part; }
             }
              return (NODE***)NULL;
            }
          if (sign < 0)
            # key < KEYOF(tree->nodedata.value)  --> Suche im linken Teilbaum:
            { *stack_ptr++ = &tree->nodedata.left; tree = tree->nodedata.left; }
            else
            # key > KEYOF(tree->nodedata.value)  --> Suche im rechten Teilbaum:
            { *stack_ptr++ = &tree->nodedata.right; tree = tree->nodedata.right; }
    }   }}
  local NODE* AVL(AVLID,delete1) (node_to_delete,tree)
    var NODE* node_to_delete;
    var NODE* tree;
    { var KEY key = KEYOF(node_to_delete->nodedata.value);
      var NODE** nodeplace = &tree;
      var NODE** stack[MAXHEIGHT]; # Ein kleiner Privat-Stack
      var uintC stack_count = 0; # Anzahl der Elemente auf dem Stack
      var NODE** * stack_ptr = &stack[0]; # stets = &stack[stack_count]
      loop
        { var NODE* node = *nodeplace;
          if (node == EMPTY) goto fertig; # Element nicht gefunden
          *stack_ptr++ = nodeplace; stack_count++;
         {var SIGNED_INT sign = COMPARE(key,KEYOF(node->nodedata.value));
          if (sign == 0)
            { var NODE** * new_stack_ptr =
                AVL(AVLID,delete1find)(node_to_delete,key,node,stack_ptr);
              if (new_stack_ptr) # oder irgendwo im Baum ab node gefunden?
                { stack_count += (new_stack_ptr - stack_ptr);
                  stack_ptr = new_stack_ptr;
                  nodeplace = stack_ptr[-1];
                  break;
                }
                else
                goto fertig; # nicht gefunden
            }
          if (sign < 0)
            # key < KEYOF(node->nodedata.value)  --> im linken Teilbaum entfernen:
            { nodeplace = &node->nodedata.left; }
            else
            # key > KEYOF(node->nodedata.value)  --> im rechten Teilbaum entfernen:
            { nodeplace = &node->nodedata.right; }
        }}
      # stack_ptr = &stack[stack_count], nodeplace = stack_ptr[-1],
     {var NODE** nodeplace_to_delete = nodeplace;
      # node_to_delete = *nodeplace_to_delete ist zu entfernen.
      if (node_to_delete->nodedata.left == EMPTY)
        # node_to_delete wird ersetzt durch node_to_delete->nodedata.right.
        { *nodeplace_to_delete = node_to_delete->nodedata.right;
          stack_ptr--; stack_count--; # doch kein rebalance bei *nodeplace_to_delete!
        }
        else
        # node_to_delete wird ersetzt durch das am weitesten rechts gelegenene
        # Element von node_to_delete->nodedata.left.
        { var NODE** * stack_ptr_to_delete = stack_ptr;
          var NODE** nodeplace = &node_to_delete->nodedata.left;
          var NODE* node;
          loop
            { node = *nodeplace;
              if (node->nodedata.right == EMPTY) break;
              *stack_ptr++ = nodeplace; stack_count++;
              nodeplace = &node->nodedata.right;
            }
          *nodeplace = node->nodedata.left;
          # node nimmt die Stellung von node_to_delete ein:
          node->nodedata.left = node_to_delete->nodedata.left;
          node->nodedata.right = node_to_delete->nodedata.right;
          node->nodedata.height = node_to_delete->nodedata.height;
          *nodeplace_to_delete = node; # statt node_to_delete
          # Der Rebalance-Stack (Weg von der Wurzel nach unten) führt jetzt
          # nicht mehr über node_to_delete, sondern über node:
          *stack_ptr_to_delete = &node->nodedata.left; # statt &node_to_delete->nodedata.left
     }  }
      AVL(AVLID,rebalance)(stack_ptr,stack_count);
      fertig:
      return tree;
    }
#endif

# Macros zum Durchlaufen eines AVL-Baumes:
# AVL_map(tree,node,statement);
# Ein Baum wird durchlaufen, jeweils node gebunden und statement ausgeführt.
  # Durchlaufungsreihenfolge:
  #               AVL_map : in Reihenfolge  L N R
  #     N         AVL_map_reverse : in umgekehrter Reihenfolge  R N L
  #    / \        AVL_map_preorder : in Präfix-Reihenfolge  N L R
  #   L   R       AVL_map_postorder : in Postfix-Reihenfolge  L R N
  #
  TYPEDEF struct { NODE* node; boolean rightp; } AVL(AVLID,mapstackitem);
  TYPEDEF AVL(AVLID,mapstackitem) AVL(AVLID,mapstack)[MAXHEIGHT];
  #define AVL_map(tree,nodevar,statement)  \
    { var NODE* nodevar = (tree);                                    \
      var AVL(AVLID,mapstack) stack; # Ein kleiner Privat-Stack      \
      var uintC stack_count = 0; # Anzahl der Elemente auf dem Stack \
      var AVL(AVLID,mapstackitem) * stack_ptr = &stack[0]; # stets = &stack[stack_count] \
      GENTAG(down): # rekursiv absteigen                             \
        if (nodevar == EMPTY) goto GENTAG(up);                       \
        stack_ptr->node = nodevar;                                   \
        stack_ptr->rightp = FALSE; nodevar = nodevar->nodedata.left; \
        stack_ptr++; stack_count++;                                  \
        goto GENTAG(down);                                           \
      GENTAG(up): # wieder hochsteigen                               \
        if (stack_count == 0) goto GENTAG(end);                      \
        stack_count--; stack_ptr--;                                  \
        if (stack_ptr->rightp) goto GENTAG(up);                      \
        nodevar = stack_ptr->node;                                   \
        statement;                                                   \
        stack_ptr->rightp = TRUE; nodevar = nodevar->nodedata.right; \
        stack_ptr++; stack_count++;                                  \
        goto GENTAG(down);                                           \
      GENTAG(end): ; # fertig                                        \
    }
  #define AVL_map_reverse(tree,nodevar,statement)  \
    { var NODE* nodevar = (tree);                                    \
      var AVL(AVLID,mapstack) stack; # Ein kleiner Privat-Stack      \
      var uintC stack_count = 0; # Anzahl der Elemente auf dem Stack \
      var AVL(AVLID,mapstackitem) * stack_ptr = &stack[0]; # stets = &stack[stack_count] \
      GENTAG(down): # rekursiv absteigen                             \
        if (nodevar == EMPTY) goto GENTAG(up);                       \
        stack_ptr->node = nodevar;                                   \
        stack_ptr->rightp = TRUE; nodevar = nodevar->nodedata.right; \
        stack_ptr++; stack_count++;                                  \
        goto GENTAG(down);                                           \
      GENTAG(up): # wieder hochsteigen                               \
        if (stack_count == 0) goto GENTAG(end);                      \
        stack_count--; stack_ptr--;                                  \
        if (!(stack_ptr->rightp)) goto GENTAG(up);                   \
        nodevar = stack_ptr->node;                                   \
        statement;                                                   \
        stack_ptr->rightp = FALSE; nodevar = nodevar->nodedata.left; \
        stack_ptr++; stack_count++;                                  \
        goto GENTAG(down);                                           \
      GENTAG(end): ; # fertig                                        \
    }
  #define AVL_map_preorder(tree,nodevar,statement)  \
    { var NODE* nodevar = (tree);                                    \
      var AVL(AVLID,mapstack) stack; # Ein kleiner Privat-Stack      \
      var uintC stack_count = 0; # Anzahl der Elemente auf dem Stack \
      var AVL(AVLID,mapstackitem) * stack_ptr = &stack[0]; # stets = &stack[stack_count] \
      GENTAG(down): # rekursiv absteigen                             \
        if (nodevar == EMPTY) goto GENTAG(up);                       \
        statement;                                                   \
        stack_ptr->node = nodevar;                                   \
        stack_ptr->rightp = FALSE; nodevar = nodevar->nodedata.left; \
        stack_ptr++; stack_count++;                                  \
        goto GENTAG(down);                                           \
      GENTAG(up): # wieder hochsteigen                               \
        if (stack_count == 0) goto GENTAG(end);                      \
        stack_count--; stack_ptr--;                                  \
        if (stack_ptr->rightp) goto GENTAG(up);                      \
        nodevar = stack_ptr->node;                                   \
        stack_ptr->rightp = TRUE; nodevar = nodevar->nodedata.right; \
        stack_ptr++; stack_count++;                                  \
        goto GENTAG(down);                                           \
      GENTAG(end): ; # fertig                                        \
    }
  #define AVL_map_postorder(tree,nodevar,statement)  \
    { var NODE* nodevar = (tree);                                    \
      var AVL(AVLID,mapstack) stack; # Ein kleiner Privat-Stack      \
      var uintC stack_count = 0; # Anzahl der Elemente auf dem Stack \
      var AVL(AVLID,mapstackitem) * stack_ptr = &stack[0]; # stets = &stack[stack_count] \
      GENTAG(down): # rekursiv absteigen                             \
        if (nodevar == EMPTY) goto GENTAG(up);                       \
        stack_ptr->node = nodevar;                                   \
        stack_ptr->rightp = FALSE; nodevar = nodevar->nodedata.left; \
        stack_ptr++; stack_count++;                                  \
        goto GENTAG(down);                                           \
      GENTAG(up): # wieder hochsteigen                               \
        if (stack_count == 0) goto GENTAG(end);                      \
        stack_count--; stack_ptr--;                                  \
        nodevar = stack_ptr->node;                                   \
        if (stack_ptr->rightp) { statement; goto GENTAG(up); }       \
        stack_ptr->rightp = TRUE; nodevar = nodevar->nodedata.right; \
        stack_ptr++; stack_count++;                                  \
        goto GENTAG(down);                                           \
      GENTAG(end): ; # fertig                                        \
    }

# Beispiel zur Anwendung von AVL(AVLID,least) und AVL(AVLID,move):
#   { var NODE* tree = ...;
#     var KEY limit = ...;
#     # Suche in tree nach dem kleinsten Key >= limit:
#     var AVL(AVLID,stack) stack;
#     var NODE* bestfit = AVL(AVLID,least)(limit,&tree,&stack);
#     if (bestfit == EMPTY) { error(); }
#     # Nun ist sicher COMPARE(KEYOF(bestfit->nodedata.value),limit) >= 0.
#     ...; KEYOF(bestfit->nodedata.value) -= limit; ...;
#     # gefundenes und modifiziertes Element im AVL-Baum umhängen:
#     AVL(AVLID,move)(&stack);
#   }

TYPEDEF struct { uintC count; NODE** path[MAXHEIGHT]; } AVL(AVLID,stack);

# Liefert das Element aus einem AVL-Baum, dessen Key möglichst klein, aber
# noch >= ein gegebener Limit ist. (EMPTY, falls alle Elemente < Limit sind.)
# Dazu als Vorbereitung fürs Löschen den Pfad von der Wurzel bis dorthin
# (inclusive, d.h. Ergebnis = stack->path[stack->count-1] ).
#ifndef NO_AVL_LEAST
  local NODE* AVL(AVLID,least) (KEY limit, NODE** tree_ptr, AVL(AVLID,stack) * stack);
  local NODE* AVL(AVLID,least) (
    var KEY limit,
    var NODE** tree_ptr,
    var AVL(AVLID,stack) * stack) # Ausgabe: Pfad von der Wurzel ab
    { var NODE* mark = EMPTY;
      var uintC markdepth = 0;
      var NODE** nodeplace = tree_ptr;
      var uintC nodedepth = 0;
      # mark = betrachteter Teilbaum, node = letztes betrachtetes Element darin.
      # markdepth = Stacktiefe bis mark, nodedepth = Stacktiefe bis node.
      # Es gilt markdepth <= nodedepth.
      loop
        { stack->path[nodedepth++] = nodeplace;
         {var NODE* node = *nodeplace;
          # Alle Elemente mit Key >= Limit liegen entweder im Teilbaum
          # unterhalb von node oder rechts von mark (mark eingeschlossen).
          if (node==EMPTY) break;
          if (COMPARE(KEYOF(node->nodedata.value),limit) < 0)
            { # Alle Elemente unterhalb von node, die >= Limit sind, müssen
              # bereits unterhalb von node->nodedata.right liegen.
              nodeplace = &node->nodedata.right;
            }
            else
            { # Limit <= node <= mark.
              # Ab jetzt nur den Teilbaum unterhalb von node betrachten:
              mark = node; markdepth = nodedepth;
              nodeplace = &node->nodedata.left;
            }
        }}
      # Alle Elemente >= Limit liegen rechts von mark (mark eingeschlossen).
      stack->count = markdepth; return mark;
    }
#endif

# Setzt ein Element in einem AVL-Baum um, nachdem sich sein Key verändert hat.
#ifndef NO_AVL_MOVE
  local void AVL(AVLID,move) (AVL(AVLID,stack) * stack);
  local void AVL(AVLID,move) (
    var AVL(AVLID,stack) * stack) # Ein kleiner Stack
    { var uintC stack_count = stack->count; # Anzahl der Elemente auf dem Stack
      var NODE** * stack_ptr = &stack->path[stack_count]; # stets = &stack->path[stack_count]
      # 1. Schritt, vgl. AVL(AVLID,delete) :
      var NODE** nodeplace_to_delete = stack_ptr[-1];
      var NODE* node_to_delete = *nodeplace_to_delete; # zu entfernendes Element
      # node_to_delete = *nodeplace_to_delete ist zu entfernen.
      if (node_to_delete->nodedata.left == EMPTY)
        # node_to_delete wird ersetzt durch node_to_delete->nodedata.right.
        { *nodeplace_to_delete = node_to_delete->nodedata.right;
          stack_ptr--; stack_count--; # doch kein rebalance bei *nodeplace_to_delete!
        }
        else
        # node_to_delete wird ersetzt durch das am weitesten rechts gelegenene
        # Element von node_to_delete->nodedata.left.
        { var NODE** * stack_ptr_to_delete = stack_ptr;
          var NODE** nodeplace = &node_to_delete->nodedata.left;
          var NODE* node;
          loop
            { node = *nodeplace;
              if (node->nodedata.right == EMPTY) break;
              *stack_ptr++ = nodeplace; stack_count++;
              nodeplace = &node->nodedata.right;
            }
          *nodeplace = node->nodedata.left;
          # node nimmt die Stellung von node_to_delete ein:
          node->nodedata.left = node_to_delete->nodedata.left;
          node->nodedata.right = node_to_delete->nodedata.right;
          node->nodedata.height = node_to_delete->nodedata.height;
          *nodeplace_to_delete = node; # statt node_to_delete
          # Der Rebalance-Stack (Weg von der Wurzel nach unten) führt jetzt
          # nicht mehr über node_to_delete, sondern über node:
          *stack_ptr_to_delete = &node->nodedata.left; # statt &node_to_delete->nodedata.left
        }
      AVL(AVLID,rebalance)(stack_ptr,stack_count);
      # 2. Schritt, vgl. AVL(AVLID,insert) :
     {var KEY key = KEYOF(node_to_delete->nodedata.value);
      var NODE** nodeplace = stack->path[0]; # = &tree
      stack_count = 0; stack_ptr = &stack->path[0];
      loop
        { var NODE* node = *nodeplace;
          if (node == EMPTY) break;
          *stack_ptr++ = nodeplace; stack_count++;
          if (COMPARE(key,KEYOF(node->nodedata.value)) < 0)
            # key < KEYOF(node->nodedata.value)  --> im linken Teilbaum einfügen:
            { nodeplace = &node->nodedata.left; }
            else
            # key >= KEYOF(node->nodedata.value)  --> im rechten Teilbaum einfügen:
            { nodeplace = &node->nodedata.right; }
        }
      node_to_delete->nodedata.left = EMPTY;
      node_to_delete->nodedata.right = EMPTY;
      node_to_delete->nodedata.height = 1;
      *nodeplace = node_to_delete;
      AVL(AVLID,rebalance)(stack_ptr,stack_count);
    }}
#endif

# Sortiert einen AVL-Baum, nachdem sich die Keys verändert haben, und
# liefert den neuen AVL-Baum.
#ifndef NO_AVL_SORT
  local NODE* AVL(AVLID,sort) (NODE* tree);
  local NODE* AVL(AVLID,sort) (tree)
    var NODE* tree;
    { var NODE* new_tree = EMPTY;
      AVL_map_postorder(tree,node, new_tree = AVL(AVLID,insert1)(node,new_tree); );
      return new_tree;
    }
#endif

#ifdef DEBUG_AVL
# Gibt einen AVL-Baum aus.
# Benutzt asciz_out() und hex_out().
  local void AVL(AVLID,out) (NODE* tree);
  local void AVL(AVLID,out)(tree)
    var NODE* tree;
    { if (!(tree==EMPTY))
        { asciz_out("(");
          if (!(tree->nodedata.left==EMPTY))
            { AVL(AVLID,out)(tree->nodedata.left); asciz_out("<"); }
          hex_out(tree);
          if (!(tree->nodedata.right==EMPTY))
            { asciz_out(">"); AVL(AVLID,out)(tree->nodedata.right); }
          asciz_out(")");
    }   }
#endif

#ifdef DEBUG_AVL
  # Invarianten eines AVL-Baumes überprüfen:
  local void AVL(AVLID,check) (NODE* tree);
  local void AVL(AVLID,checkleft) (NODE* tree, KEY key);
  local void AVL(AVLID,checkright) (NODE* tree, KEY key);
  local void AVL(AVLID,check) (tree)
    var NODE* tree;
    { # Überprüfe Regeln 1 und 2:
      AVL_map_postorder(tree,node,
        { var HEIGHT h = node->nodedata.height;
          var HEIGHT hl = heightof(node->nodedata.left);
          var HEIGHT hr = heightof(node->nodedata.right);
          if (!(   ((h == hl+1) && (hr <= hl) && (hl <= hr+1))
                || ((h == hr+1) && (hl <= hr) && (hr <= hl+1))
             ) )
            abort();
        });
      # Überprüfe Regel 3:
      AVL_map(tree,node,
        { AVL(AVLID,checkleft)(node->nodedata.left,KEYOF(node->nodedata.value));
          AVL(AVLID,checkright)(node->nodedata.right,KEYOF(node->nodedata.value));
        });
    }
  # Überprüfe, ob alle Elemente von tree einen Wert <= key haben:
  local void AVL(AVLID,checkleft) (tree,key)
    var NODE* tree;
    var KEY key;
    { AVL_map(tree,node,
        if (!( COMPARE(KEYOF(node->nodedata.value),key) <= 0)) abort();
        );
    }
  # Überprüfe, ob alle Elemente von tree einen Wert >= key haben:
  local void AVL(AVLID,checkright) (tree,key)
    var NODE* tree;
    var KEY key;
    { AVL_map(tree,node,
        if (!( COMPARE(KEYOF(node->nodedata.value),key) >= 0)) abort();
        );
    }
#endif

#undef heightof
#undef TYPEDEF

# ------------------------------------------------------------------------------

#endif

