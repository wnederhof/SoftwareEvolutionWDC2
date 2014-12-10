module tests::Tests

import IO;
import String;
import Prelude;
import lang::java::jdt::m3::AST;
import lang::java::jdt::m3::Core;

import clonedetectors::Type1CloneDetector;
import clonedetectors::Type2CloneDetector;
import utils::Utils;
import Main;

loc project = |project://HelloWorld|;

//Test to check addStatement to the bucket
test bool testAddStatementToBucket(){
	
	map[value, list[tuple[loc,value]]] result = ();
	set[Declaration] ast = createAstsFromEclipseProject(project, true);
	
	for (Declaration d <- ast) {
		visit (d) {
			case Statement st:{
				if(countStatements(st) >= 5)
					result = addStatementToBucket(st, result);
			}
		}
	}
	
	return (size(result) == 10);
}

//Test to check addDeclaration to the bucket
test bool testAddDeclarationToBucket(){	

	map[value, list[tuple[loc,value]]] result = ();
	set[Declaration] ast = createAstsFromEclipseProject(project, true);
	
	for (Declaration d <- ast) {
		visit (d) {
			case Declaration dec:
			{
				switch(dec){
					case \method(_, _, _, _, Statement impl) :
					{	
						if(countStatements(impl) >= 5)
							result = addDeclarationToBucket(dec, result);
					}	
					case \constructor(_, _, _ , Statement impl) :
					{
						if(countStatements(impl) >= 5)
							result = addDeclarationToBucket(dec, result);
					}
				}
			}
		}
	}
	return (size(result) == 8);
}

//Test to check if the subtree clones are well calculated
test bool testCalculateSubtreeClones(){
	
	num result = 1;
	set[Declaration] ast = createAstsFromEclipseProject(project, true);
	
	return (size(calculateSubtreeClones(ast,5)) == result); 
	
}



//Test to check if getMethodsWithMetrics obtains the correct number of methods 
test bool testGetMethods(){
	
	//loc project = |project://|;
	set[Declaration] ast = createAstsFromEclipseProject(project, true);
	
	//Test project must return 8 methods
	return (size(getMethodsWithMetrics(ast)) == 8);
}

//Test to validate CreateMetrics
test bool testCreateMetrics(){
	map[str, int] result = ();
	
	result += ("declarationStatements":0);
	result += ("executableStatements":0);
	result += ("callsToOtherFunctions":0);
	result += ("uniqueCallsToOtherFunctions":0);
	result += ("arcs":0);
	result += ("exit":0);
	result += ("statements":0);
	result += ("if":0);
	result += ("do":0);
	result += ("while":0);
	result += ("for":0);
	result += ("foreach":0);
	result += ("switch":0);
	result += ("case":0);
	
	result += ("expressionStatement":0);
	
	return (result == createMetrics());
}

//Test to check the metrics
test bool testCalculateMetrics(){
	
	map[str, int] metrics = ();
	set[Declaration] ast = createAstsFromEclipseProject(project, true);
	
	for (Declaration d <- ast) {
		visit (d){
			case \method(Type \return, str name, list[Declaration] parameters, list[Expression] exceptions, Statement impl):
			{	
				if (countStatements(impl) >= 10){
					metrics = calculateMetrics(impl);
				}
			}
		}
	}
	
	map[str, int] result = ();
	result = ("arcs":0,
	"statements":15,
	"for":1,
	"do":0,
	"executableStatements":13,
	"case":0,
	"while":1,
	"switch":0,
	"expressionStatement":2,
	"callsToOtherFunctions":3,
	"if":1,
	"foreach":0,
	"uniqueCallsToOtherFunctions":2,
	"exit":1,
	"declarationStatements":2);
	
	
	return (result == metrics); 
}

//test validate getClones
test bool testGetClones(){

	int result = 1;
	
	map[str, int] mapInput1 = ();
	mapInput1 += ("declarationStatements":1);
	mapInput1 += ("executableStatements":2);
	mapInput1 += ("callsToOtherFunctions":3);
	mapInput1 += ("uniqueCallsToOtherFunctions":0);
	mapInput1 += ("arcs":2);
	mapInput1 += ("exit":1);
	mapInput1 += ("statements":4);
	mapInput1 += ("if":4);
	mapInput1 += ("do":2);
	mapInput1 += ("while":1);
	mapInput1 += ("for":3);
	mapInput1 += ("foreach":2);
	mapInput1 += ("switch":1);
	mapInput1 += ("case":1);
	mapInput1 += ("expressionStatement":0);
	
	map[str, int] mapInput2 = ();
	mapInput2 += ("declarationStatements":1);
	mapInput2 += ("executableStatements":2);
	mapInput2 += ("callsToOtherFunctions":3);
	mapInput2 += ("uniqueCallsToOtherFunctions":0);
	mapInput2 += ("arcs":2);
	mapInput2 += ("exit":1);
	mapInput2 += ("statements":4);
	mapInput2 += ("if":4);
	mapInput2 += ("do":2);
	mapInput2 += ("while":1);
	mapInput2 += ("for":3);
	mapInput2 += ("foreach":2);
	mapInput2 += ("switch":1);
	mapInput2 += ("case":1);
	mapInput2 += ("expressionStatement":0);
	
	tuple[loc,value] tuple1 = <|project://loc1/1.java|,"value1">;
	tuple[loc,value] tuple2 = <|project://loc1/2.java|,"value2">; 
	map[tuple[loc,value], map[str,int]] input = (
	tuple1 : mapInput1,
	tuple2 : mapInput2
	);
	
	return (size(getClones(input, 2)) == result);	
}


//UTILS
test bool testReplaceByWhiteSpaces() {
	str testStr = "abcdefg";
	return assert(replaceByWhiteSpaces(testStr,1,3) == "a   efg");
}


test bool testCalculateBlockSize(){

	set[Declaration] ast = createAstsFromEclipseProject(project, true);
	M3 model = createM3FromEclipseProject(project);
	
	clones = calculateSubtreeClones(ast,5);

	list[num] cloneSizes = [];
	for(clone <- clones){
		for (c <- clone)
			cloneSizes += [calcBlockSize(c[0], model)];
	}
	
	return (cloneSizes == [12,12]);
}

test bool testCalculateCompilationUnitVolume(){
	
	M3 model = createM3FromEclipseProject(project);
	compilationUnits = {l[0] | l <- model@containment, isCompilationUnit(l[0])};
	
	num result = sum([calcCompilationUnitVol(l, model) | l <- compilationUnits]);
	
	return (result == 94);
}

test bool testVolume() {
	model = createM3FromEclipseProject(project);
	volume = calculateVolume(model);
	
	return ((volume > 90) && (volume < 110));
}


test bool testCalculateSizeClone(){

	set[Declaration] ast = createAstsFromEclipseProject(project, true);
	M3 model = createM3FromEclipseProject(project);
	
	clones = calculateSubtreeClones(ast,5);
	
	return (calculateClonesMetrics(clones,model) == <24,12>);	
}

/*This test veryfies if the method countStatements is returning the number of 
*statements correctly
*/
test bool testCountStatements(){

	set[Declaration] ast = createAstsFromEclipseProject(project, true);
	
	list[num] statements = [];
	
	for (Declaration d <- ast) {
		visit (d){
			case \method(Type \return, str name, list[Declaration] parameters, list[Expression] exceptions, Statement impl):
			{
				//we get the list of valid statements of each method 
				statements += [countStatements(impl)];
			}
		}
	}
	
	//The list of statements of each method is: [5,16,5,6,5,5,9,9]
	list[num] result = [5,16,5,6,5,5,9,9];
	
	return (result == statements);
}

//Main
test bool testGetClonesExample(){
	
	set[Declaration] ast = createAstsFromEclipseProject(project, true);
	
	clonesT1 = calculateSubtreeClones(ast,5);
	clonesT2 = calculateClonesT2(ast, 2);
	
	map[int, set[set[tuple[loc,value]]]] allClones = ();
	allClones[1] = clonesT1;
	allClones[2] = clonesT2;
	
	result = (1:[<"YHello.java","{\n\t\tint i = 0;\n\t\ttry {\n\t\t\ti = 1;\n\t\t} catch (Exception e) {\n\t\t\ti = 2;\n\t\t\tthrow new RuntimeException(e);\n\t\t} catch (OutOfMemoryError e) {\n\t\t\ti = 3;\n\t\t}\n\t\treturn 0;\n\t}">,<"YHello.java","{\n\t\tint i = 0;\n\t\ttry {\n\t\t\ti = 1;\n\t\t} catch (Exception e) {\n\t\t\ti = 2;\n\t\t\tthrow new RuntimeException(e);\n\t\t} catch (OutOfMemoryError e) {\n\t\t\ti = 3;\n\t\t}\n\t\treturn 0;\n\t}">],2:[<"YHello.java","{\n\t\tint i = 0;\n\t\t\n\t\twhile (i \< 10)\n\t\t{\n\t\t\tString j = \"\";\n\t\t\ti = i+1;\n\t\t}\n\t\t\n\t\td();\n\t}">,<"YHello.java","{\n\t\tint j = 0;\n\t\t\n\t\twhile (j \< 10)\n\t\t{\n\t\t\tString k = \"\";\n\t\t\tj = j+1;\n\t\t}\n\t\t\n\t\td();\n\t}">]);
	return ( getCloneExamples(allClones) == result);
}


test bool testGetCloneMetrics(){
	
	set[Declaration] ast = createAstsFromEclipseProject(project, true);
	
	clonesT1 = calculateSubtreeClones(ast,5);
	clonesT2 = calculateClonesT2(ast, 2);
	
	map[int, set[set[tuple[loc,value]]]] allClones = ();
	allClones[1] = clonesT1;
	allClones[2] = clonesT2;
	 
	result = (1:<26.00,1,12,2>,2:<19.00,1,9,2>);
	return (result == getCloneMetrics(allClones, project));

}



