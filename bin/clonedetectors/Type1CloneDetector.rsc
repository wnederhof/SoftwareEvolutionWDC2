module clonedetectors::Type1CloneDetector
import Prelude;
import lang::java::jdt::m3::AST;
import Traversal;
import Node;
import utils::LocCalculator;

map[value, list[tuple[loc,value]]] bucket = ();

/*
* Compute the size of a statement
* TODO : should be improved because the results are not accurate
*/

private num countStatements(Statement impl){
	num result = 0;
		
		visit (impl){
			case \block(list[Statement] statements): result += (size(statements));			
			case \switch(_, list[Statement] statements): result += (size(statements));
			case \if(_, _):	result += 1;
			case \if(_, _, _): result += 2;
			case \while(_, _) : result +=1;
			case \do(_, _) : result +=2;
			case \foreach(_, _, _) : result +=1;
			case \for(_, _, _) : result += 1;
			case \for(_, _, _, _) : result +=1;
			case \try(_, list[Statement] catchClauses): result += size(catchClauses) + 1;
			case \try(_, list[Statement] catchClauses, _) : result += size(catchClauses) + 2;
		}
	
	//println(result);
	return result;
}

/*
* Add a statement to the bucket
*/
private void addStatementToBucket(Statement st)
{
	loc source = st@src;
	s = delAnnotationsRec(st);
	if (s notin bucket) {
		bucket[s] = [];
	}
	bucket[s] += [<source,st>];
}

/*
* Add a declaration to the bucket
*/
private void addDeclarationToBucket(Declaration dec)
{
	loc source = dec@src;
	d = delAnnotationsRec(dec);
	if (d notin bucket) {
		bucket[d] = [];
	}
	bucket[d] += [<source,dec>];
}

/*
* Identify AST subtree clones
*/
private set[set[tuple[loc,value]]] calculateSubtreeClones(set[Declaration] AST, int threshold) {

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
							addDeclarationToBucket(dec);
					}	
					case \constructor(_, _, _ , Statement impl) :
					{
						if(countStatements(impl) >= threshold)
							addDeclarationToBucket(dec);
					}
				}
			}
			case Statement st:
			{
				if(countStatements(st) >= threshold)
					addStatementToBucket(st);
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
	* remove clone classes that are strictly included in others
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
	
	set[set[tuple[loc,value]]] clones = {};
	for(clone <- finalClones){
		set[tuple[loc,value]] cloneEl = {};
		for(c <- finalClones[clone])
			cloneEl += {c};
		clones += {cloneEl};
	}
	
	return clones;
}


public set[set[tuple[loc,value]]] calculateClonesT1(loc project) {	
	set[Declaration] AST = createAstsFromEclipseProject(project, true);
	set[set[tuple[loc,value]]] clones = calculateSubtreeClones(AST,5);
	
	return clones;
}
