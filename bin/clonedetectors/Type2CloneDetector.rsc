module clonedetectors::Type2CloneDetector

import Prelude;
import lang::java::jdt::m3::AST;
import Node;
import Traversal;
import utils::Utils;


public set[set[tuple[loc,value]]] calculateClonesT2(set[Declaration] ast, int cloneType) {
	
	map[tuple[loc,value], map[str,int]] methods = getMethodsWithMetrics(ast);
	
	set[set[tuple[loc,value]]] clones = getClones(methods, cloneType);
	
	return clones;
}

//We get the methods that will be processed in order to get clones
public map[tuple[loc,value], map[str,int]] getMethodsWithMetrics(set[Declaration] ast)
{
	map[tuple[loc,value], map[str,int]] methods = ();

	for (Declaration d <- ast) {
		visit (d){
			case \method(Type \return, str name, list[Declaration] parameters, list[Expression] exceptions, Statement impl):
			{	
				//if the method has more or equal than 5 statements
				//this is to avoid getters and setters
				if (countStatements2(impl) >= 5){
					map[str, int] metrics = calculateMetrics(impl);
					methods += (<impl@src,delAnnotationsRec(impl)>: metrics);
				}				
			}			
		}
	}
	
	return methods;
}

//Initializes a map with the metrics and the score
public map[str, int] createMetrics(){

	map[str, int] result = ();
	
	result += ("declarationStatements":0);
	result += ("executableStatements":0);
	
	result += ("callsToOtherFunctions":0);
	result += ("uniqueCallsToOtherFunctions":0);
	
	result += ("arcs":0);
	result += ("exit":0); // breaks and returns.
	
	//declaration statements + executable statements
	result += ("statements":0);
	
	result += ("if":0);
	result += ("do":0);
	result += ("while":0);
	result += ("for":0);
	result += ("foreach":0);
	result += ("switch":0);
	result += ("case":0);
	
	result += ("expressionStatement":0);
	
	return result;
}

//Here we count and set the score for each metric of each method
public map[str, int] calculateMetrics(Statement statement)
{
	map[str, int] result = createMetrics();
	set[str] unicMethodsCall = {};
	
	for (value s <- statement){
		visit (s){
			case \methodCall(bool isSuper, str name, list[Expression] arguments):{
				result["callsToOtherFunctions"] += 1;
				
				unicMethodsCall += {name};
				result["executableStatements"] +=1;
			}
	    
			case \declarationStatement(Declaration declaration):{
				result["declarationStatements"] += 1;
			}
			
			case \break():{	
				result["exit"] += 1;
				result["executableStatements"] +=1;
			}
			
			case \break(str label):{
				result["exit"] += 1;
				result["executableStatements"] +=1;
			}
			
			case \return():{	
				result["exit"] += 1;
				result["executableStatements"] +=1;
			}
			
			case \return(Expression expression):{
				result["exit"] += 1;
				result["executableStatements"] +=1;
			}
		
			case \if(Expression condition, Statement thenBranch):{
				result["if"] += 1;
				result["executableStatements"] +=1;
			}
					
			case \if(Expression condition, Statement thenBranch, Statement elseBranch):{
				result["if"] += 1;
				result["executableStatements"] +=1;
			}
			
			case \do(Statement body, Expression condition):{
				result["do"] += 1;
				result["executableStatements"] +=1;
			}
			
			case \while(Expression condition, Statement body):{
				result["while"] += 1;
				result["executableStatements"] +=1;
			}
			
			case \for(list[Expression] initializers, Expression condition, list[Expression] updaters, Statement body):{
	    		result["for"] += 1;
	    		result["executableStatements"] +=1;
	    	}
	    		
			case \for(list[Expression] initializers, list[Expression] updaters, Statement body):{
				result["for"] += 1;
				result["executableStatements"] +=1;
			}
			
			case \foreach(Declaration parameter, Expression collection, Statement body):{
				result["foreach"] += 1;
				result["executableStatements"] +=1;		
			}
			
			case \switch(Expression expression, list[Statement] statements):{
				result ["switch"] += 1;
				result["executableStatements"] +=1;
			}
			
			case \case(Expression expression):{
				result ["case"] += 1;
				result["executableStatements"] +=1;
			}
			
			case \assignment(Expression lhs, str operator, Expression rhs):{
				result["executableStatements"] +=1;
			}
			
			case \methodCall(bool isSuper, Expression receiver, str name, list[Expression] arguments):{	
				
				result["expressionStatement"] += 1;			
				result["executableStatements"] +=1;
			}
		}
		
		result["statements"] = result["declarationStatements"] + 
								result["executableStatements"];
		result["uniqueCallsToOtherFunctions"] = size(unicMethodsCall);
	}
		
	return result;
}

//Compare the every method in the map to obtain the clones
public set[set[tuple[loc,value]]] getClones(map[tuple[loc,value], map[str,int]] methods, int cloneType) {
	
	set[set[tuple[loc,value]]] result = {};	
	
	//{{a,b},{c,d}} these are clone classes {a,b} and {c,d}
	set[set[tuple[loc,value]]] cloneClassesT1 = {};
	set[set[tuple[loc,value]]] cloneClassesT2 = {};
	
	for (m <- methods){
		
		set[tuple[loc,value]] classesT1 = {m};
		set[tuple[loc,value]] classesT2 = {m};
		
		for (i <- methods){
		
			//Whether the tuple M is different than the tuple I
			//i.e Not the same method
			if (m != i)
			{
				//Type I (if needed) compares the content of the method
				if (m[1] == i[1])
					classesT1 += i;
				
				//Type II compares the metrics of the methods
				else if (methods[m] == methods[i])
					classesT2 += i;
			}
		}
		
		//Adds the clones to the results
		if (size(classesT1) > 1){
			cloneClassesT1 += {classesT1};
		}
		
		if (size(classesT2) > 1)
			cloneClassesT2 += {classesT2};

	}
	

	/*println("number of T1s : <size(cloneClassesT1)>");
	for (c <- cloneClassesT1){
		println("size of C1 <size(c)>");
	}
			
		
	println("number of T2s : <size(cloneClassesT2)>");	
	for (c <- cloneClassesT2){
		println("size of C2 <size(c)>");
	}
	*/
	
	//Returning code clones of the type needed
	if (cloneType == 1)
		result = cloneClassesT1;
	else if (cloneType == 2)
		result = cloneClassesT2;
		
		
		
	return result;
}
