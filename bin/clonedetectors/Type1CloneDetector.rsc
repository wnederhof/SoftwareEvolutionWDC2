module clonedetectors::Type1CloneDetector

import Prelude;
import lang::java::jdt::m3::AST;
import Traversal;
import Node;
import utils::Utils;

/*
* Add a statement to the bucket
*/
public map[value, list[tuple[loc,value]]] addStatementToBucket(Statement st, map[value, list[tuple[loc,value]]] bucket)
{
	loc source = st@src;
	s = delAnnotationsRec(st);
	if (s notin bucket) {
		bucket[s] = [];
	}
	bucket[s] += [<source,st>];
	
	return bucket;
}

/*
* Add a declaration to the bucket
*/
public map[value, list[tuple[loc,value]]] addDeclarationToBucket(Declaration dec,map[value, list[tuple[loc,value]]] bucket)
{
	loc source = dec@src;
	d = delAnnotationsRec(dec);
	if (d notin bucket) {
		bucket[d] = [];
	}
	bucket[d] += [<source,dec>];
	
	return bucket;
}

/*
* Identify AST subtree clones
*/
public set[set[tuple[loc,value]]] calculateSubtreeClones(set[Declaration] AST, int threshold) {
	
	map[value, list[tuple[loc,value]]] bucket = ();
	map[value, list[tuple[loc,value]]] tempClones = ();
	map[value, list[tuple[loc,value]]] finalClones = ();
	
	/*
	* Detect all statements or declarations that have a size 
	* higher than the specified threshold
	*/
	for (Declaration d <- AST) {
		visit (d) {
			case Declaration dec:
			{
				switch(dec){
					case \method(_, _, _, _, Statement impl) :
					{	
						if(countStatements(impl) >= threshold)
							bucket = addDeclarationToBucket(dec, bucket);
					}	
					case \constructor(_, _, _ , Statement impl) :
					{
						if(countStatements(impl) >= threshold)
							bucket = addDeclarationToBucket(dec, bucket);
					}
				}
			}
			case Statement st:
			{
				if(countStatements(st) >= threshold)
					bucket = addStatementToBucket(st, bucket);
			}
		}
	}
	
	
	/*
	* get all clone classes
	*/
	for (s <- bucket)
		if(size(bucket[s]) > 1)
			tempClones[s] = bucket[s];
	
	finalClones = tempClones;
	
	/*
	* remove clone classes that are strictly included in others (subsumption)
	*/
	for (c <- tempClones){
		visit(tempClones[c][0][1]){
			case Declaration d:
			{
				if(d != tempClones[c][0][1]){
					d = delAnnotationsRec(d);
					if(d in finalClones)
						finalClones = delete(finalClones,d);
				}
			}
			case Statement s:
			{
				if(s != tempClones[c][0][1]){
					s = delAnnotationsRec(s);
					if(s in finalClones)
						finalClones = delete(finalClones, s);
				}
			}		
		}
	}	
	
	/*
	* create the final structure of clones to be returned
	*/
	set[set[tuple[loc,value]]] clones = {};
	for(clone <- finalClones){
		set[tuple[loc,value]] cloneEl = {};
		for(c <- finalClones[clone])
			cloneEl += {c};
		clones += {cloneEl};
	}
	
	return clones;
}


public set[set[tuple[loc,value]]] calculateClonesT1(set[Declaration] ast) {	
	
	set[set[tuple[loc,value]]] clones = calculateSubtreeClones(ast,5);
	
	return clones;
}
