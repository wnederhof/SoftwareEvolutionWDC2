module tests::Tests

import IO;
import String;
import Prelude;

import clonedetectors::Type1CloneDetector;
import clonedetectors::Type2CloneDetector;

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


test bool testGetMethods(){}
test bool testCountStatements(){}
test bool testCreateMetrics(){}
test bool testCalculateMetrics(){}
test bool testGetClones(){}







void testAll() {
	println("Testing Type 1 Clone Detection");
	testType1();
	
	println("Testing Type 2 Clone Detection");
	testType2();
}