module Main

import clonedetectors::Type1CloneDetector;
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
	         
	/*for (c <- clones) {
		println("clone class size <size(clones[c])>");
		for (clone <- clones[c]){
			println(clone[1]);
		}
	}*/
}