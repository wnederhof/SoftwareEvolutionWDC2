module Main

import clonedetectors::Type1CloneDetector;
import clonedetectors::Type2CloneDetector;
import Prelude;
import utils::LocCalculator;
import lang::java::jdt::m3::Core;

public void main(loc project) {
	/*
	* Type 1 clones
	*/
	clones = calculateClones(project);
	biggestCloneClass = max([size(clones[c]) | c <- clones]);
	
	model = createM3FromEclipseProject(project);
	totalLOCs = calculateVolume(model);
	
	cloneMetrics = calculateClonesMetrics(clones, model);
	dupLOCsPerc = cloneMetrics[0]/totalLOCs * 100;
	println("\nType 1 Clones
			 'Duplicated lines : <dupLOCsPerc>%
	         'Number of clone classes : <size(clones)>
	         'Biggest clone: <cloneMetrics[1]> LOCs
	         'Biggest clone class: <biggestCloneClass>
	         ");
	
	/*
	*Type 2 clones
	*/
	clonesT2 = calculateClonesT2(project, 2);
	
	biggestCloneClassT2 = max([ size(c) | c <- clonesT2]);
	
	println("\nType 2 Clones
			 'Duplicated lines : UNDEFINED
	         'Number of clone classes : <size(clonesT2)>
	         'Biggest clone: UNDEFINED LOCs
	         'Biggest clone class: <biggestCloneClassT2>
	         ");
	
	
	/*int numberOfClones = 0;
	int biggestCloneClass = 0;
	
	for (c <- cloneClassesT2){
		
		int sizeCloneClass = size(c);
		
		//BiggestCloneClass
		if (size(c) > biggestCloneClass)
			biggestCloneClass = size(c);
			
		//Number of Clones
		numberOfClones += sizeCloneClass;
	}*/
	
	
	
	/*for (c <- clones) {
		println("clone class size <size(clones[c])>");
		for (clone <- clones[c]){
			println(clone[1]);
		}
	}*/
}