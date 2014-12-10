module tests::Tests

import IO;
import String;
import Prelude;
import lang::java::jdt::m3::AST;

import clonedetectors::Type1CloneDetector;
import clonedetectors::Type2CloneDetector;

loc project = |project://HelloWorld|;

void testCountStatements(){
	println("Testing countStatements...");
	
	println("OK!");
}

void testAddStatementToBucket(){
	println("Testing addStatementToBucket...");
	
	println("OK!");
}

void testAddDeclarationToBucket(){
	println("Testing addDeclarationToBucket...");
	
	println("OK!");
}

void testCalculateSubtreeClones(){
	println("Testing calculateSubtreeClones...");
	
	println("OK!");
}

void testType1(){
	testCountStatements();
	testAddStatementToBucket();
	testAddDeclarationToBucket();
	testCalculateSubtreeClones();

}

void testType2(){
	testGetMethods();
	testCountStatements();
	testCreateMetrics();
	testCalculateMetrics();
	testGetClones();

}

//Test to check if getMethodsWithMetrics obtains the correct number of methods 
test bool testGetMethods(){
	
	//loc project = |project://|;
	set[Declaration] ast = createAstsFromEclipseProject(project, true);
	
	//Test project must return XX methods
	return (size(getMethodsWithMetrics(ast)) == 7);
}


/*This test veryfies if the method countStatements is returning the number of 
*statements correctly
*/
test bool testCountStatements(){

	//loc project = |project://|;
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
	
	//The list of statements of each method is: [xx,xx]
	list[num] result = [];
	
	return (result == statements);
}


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

test bool testCalculateMetrics(){
	
	map[str, int] metrics = ();
	set[Declaration] ast = createAstsFromEclipseProject(project, true);
	
	for (Declaration d <- ast) {
		visit (d){
			case \method(Type \return, str name, list[Declaration] parameters, list[Expression] exceptions, Statement impl):
			{	
				if (countStatements(impl) >= 8){
					metrics = calculateMetrics(impl);
				}
			}
		}
	}
	
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
	
	
	return (result == metrics); 
}

test bool testGetClones(){

	int result = 2;
	
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
	
	map[tuple[str,loc,value], map[str,int]] input = (
	["testStr1","location1","value1"] : mapInput1,
	["testStr2","location2","value2"] : mapInput2
	);
	
	return (size[getClones(input, 2)] == result);	
}







void testAll() {
	println("Testing Type 1 Clone Detection");
	testType1();
	
	println("Testing Type 2 Clone Detection");
	testType2();
}