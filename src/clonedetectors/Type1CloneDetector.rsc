module clonedetectors::Type1CloneDetector
import Prelude;
import lang::java::jdt::m3::AST;
import Traversal;
import Node;
import utils::LocCalculator;

map[value, list[tuple[value,loc]]] bucket = ();

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
	bucket[s] += [<st,source>];
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
	bucket[d] += [<dec,source>];
}

/*
* Identify AST subtree clones
*/
private map[value, list[tuple[value,loc]]] calculateSubtreeClones(set[Declaration] AST, int threshold) {

	map[value, list[tuple[value,loc]]] tempClones = ();
	map[value, list[tuple[value,loc]]] finalClones = ();
	
	/*
	* Detect all statements or declarations that have a size 
	* higher than the specified threshold
	*/
	for (Declaration d <- AST) {
		visit (d) {
			case Declaration dec:
			{
				switch(dec){
					case \method(Type \return, str name, list[Declaration] parameters, list[Expression] exceptions, Statement impl) :
					{	
						if(countStatements(impl) >= threshold)
							addDeclarationToBucket(dec);
					}	
					case \constructor(str name, list[Declaration] parameters, list[Expression] exceptions, Statement impl) :
					{
						if(countStatements(impl) >= threshold)
							addDeclarationToBucket(dec);
					}
				}
			}
			case Statement st:
			{
				if(countStatements(st) >= 5)
					addStatementToBucket(st);
			}
		}
	}
	
	
	/*
	* get all clone classes
	*/
	for (s <- bucket){
		if(size(bucket[s]) > 1) {
			tempClones[s] = bucket[s];
			//noOfDupLines += sum([d[1] | d <- bucket[s]]);	
		}
	}
	
	finalClones = tempClones;
	
	/*
	* remove clone classes that are strictly included in others
	*/
	println("clones size <size(tempClones)>");
	for (c <- tempClones){
		visit(tempClones[c][0][0]){
			case Declaration d:
			{
				if(d != tempClones[c][0][0]){
					d = delAnnotationsRec(d);
					if(d in finalClones)
						finalClones = delete(finalClones,d);
				}
			}
			case Statement s:
			{
				if(s != tempClones[c][0][0]){
					s = delAnnotationsRec(s);
					if(s in finalClones)
						finalClones = delete(finalClones, s);
				}
			}		
		}
	}	
	
	return finalClones;
}


public map[value, list[tuple[value,loc]]] calculateClones(loc project) {	
	set[Declaration] AST = createAstsFromEclipseProject(project, true);
	map[value, list[tuple[value,loc]]] clones = calculateSubtreeClones(AST,5);
	
	return clones;
}
