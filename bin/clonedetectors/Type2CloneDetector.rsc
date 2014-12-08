module clonedetectors::Type2CloneDetector

import Prelude;
import lang::java::jdt::m3::AST;
import Node;


public void calculateClones(loc project) {
	
	set[Declaration] ast = createAstsFromEclipseProject(project, true);
	
	map[tuple[str,loc,value], map[str,int]] methods = getMethodsWithMetrics(ast);
	
	map[tuple[str,loc,value], map[str,int]] clones = getClones(methods);
}


private map[tuple[str,loc,value], map[str,int]] getMethodsWithMetrics(set[Declaration] ast)
{
	map[tuple[str,loc,value], map[str,int]] methods = ();

	for (Declaration d <- ast) {
		visit (d){
			case \method(Type \return, str name, list[Declaration] parameters, list[Expression] exceptions, Statement impl):
			{	
				num i = countStatements(impl);
				//println(i);
				//if (countStatements(impl) >= 5){
				if (i >= 5){
					map[str, int] metrics = CalculateMetrics(impl);
					methods += (<name,impl@src,delAnnotationsRec(impl)>: metrics);
				}				
			}			
		}
	}
	
	return methods;
}

//This must be written better
private map[str, int] createMetrics(){

	map[str, int] result = ();
	
	result += ("declarationStatements":0);
	result += ("executableStatements":0);
	
	result += ("callsToOtherFunctions":0);
	result += ("uniqueCallsToOtherFunctions":0);
	
	result += ("arcs":0); // ??
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

private map[str, int] CalculateMetrics(Statement statement)
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
			
			//case \expressionStatement(Expression stmt): 
				//println(stmt);
				
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


private map[tuple[str,loc,value], map[str,int]] getClones(map[tuple[str,loc,value], map[str,int]] methods) {
	
	map[tuple[str,loc,value], map[str,int]] result = ();	
	
	//{{a,b},{c,d}}
	set[set[tuple[str,loc,value]]] cloneClassesT1 = {};
	set[set[tuple[str,loc,value]]] cloneClassesT2 = {};
	
	for (m <- methods){
		
		set[tuple[str,loc,value]] classesT1 = {m};	
		set[tuple[str,loc,value]] classesT2 = {m};
		
		for (i <- methods){
		
			if (m != i)
			{
				//Type I
				if (m[2] == i[2])
				{
					classesT1 += i;
				}
				//Type II
				else if (methods[m] == methods[i]){
					classesT2 += i;
				}
			}
		}
		
		if (size(classesT1) > 1){
			cloneClassesT1 += {classesT1};
		}
		
		if (size(classesT2) > 1)
			cloneClassesT2 += {classesT2};

	}
	
	println("number of T1s : <size(cloneClassesT1)>");
	for (c <- cloneClassesT1){
		println("size of C1 <size(c)>");
		/*for (i <- c)
			println("<i[0]> | <i[1]>");*/
	}
			
		
	println("number of T2s : <size(cloneClassesT2)>");	
	for (c <- cloneClassesT2){
		println("size of C2 <size(c)>");
		/*for (i <- c)
			println("<i[0]> | <i[1]>");*/
	}
	
	//iprintln(cloneClassesT1);
	//println(cloneClassesT2);
	
	
	/*for (tuple[str,loc] m <- methods){
		
		//iprintln(m);
		aux[m] = methods[m];
		methods = methods - aux;
		 
		//iprintln(aux);
		
		if (aux[m] ==  getClones(methods)){
			//println(m);
			result[m] = aux[m];
		}
	}*/
	
	return result;
}

private num countStatements(Statement impl){
	num result = 0;
		
		visit (impl){
			case \block(list[Statement] statements):{
				result += (size(statements));
			}
				
				
			case \switch(Expression expression, list[Statement] statements):
				result += (size(statements));
							
			case \if(Expression condition, Statement thenBranch):
				result += 1;
				
			case \if(Expression condition, Statement thenBranch, Statement elseBranch):
				result += 2;
		}
	
	//println(result);
	return result;
}
