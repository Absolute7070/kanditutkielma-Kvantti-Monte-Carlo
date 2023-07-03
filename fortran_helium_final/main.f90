program main






    !!!!!!!!!!PLAYGROUND MODIFIICATIONS!!!!!!!
    ! - all local energies from subconfigurations saved into one big array






    !!!!!!!!!!!!!!!!!THINGS TO KNOW !!!!!!!!!!!!!!!!!!!!!!!!!!
    ! - Number of configuration subspaces is determined, how much you set thread to be 
    ! - when using evaluateexpressions, remember to trim() the expression (fparser)
    ! - Change function string length MAX_FUN_LENGTH  in fparser, when string length exceeded
    ! - for different symbols used for parameters, change variables-object 


    
    USE mt19937
    USE omp_lib
    use FortranParser, only : EquationParser
    use FortranParser_parameters, only: rn 
    

    
    implicit none

    ! measuring performance 
    real(rn) :: start_time, end_time 
    real(rn):: total_start_time, total_end_time

    ! integer kinds 
    integer, parameter :: ki8 = selected_int_kind(18)          ! double precision integer


    !!!!!!!WHEN CHANGE ATOM, CHANGE THESES!!!!!!!!!!!

    ! character length for expressions
    ! integer(ki8), parameter :: characterlengthEnergy = 402204000, characterlengthProb = 10000*100  ! character length for local energy expr 
    !                                                                                                   ! and probability expr 



    integer(ki8), parameter :: characterlengthEnergy = 50000, characterlengthProb = 300

    ! miscellaneous but important parameteters
    integer, parameter :: numberOfCoordinates = 6, numberOfParams = 2  ! number of coordinates and number of parameters 
    integer, parameter :: numberOfIterations = 1500, numberOfConfig = 1000, numberOfParticles = 2     ! number of iterations across whole configuration space and number of configurations 
   



    ! Symbolic variables 
    character(len=*), dimension(numberOfCoordinates+numberOfParams), parameter :: variables = [character(len=5) :: 'x1', 'y1', 'z1', 'x2', 'y2', 'z2', 'A2', 'A3']
   

    !parameters as values in the same order as in symbolic variables 
    real(rn), dimension(numberOfParams) :: parameterValues = [3.5993290190342768, 1.9654959356079778 ] 

    ! coordinate value range 
    real(rn), dimension(2), parameter :: coordinateValueRange = [-3, 3]



    ! thermalisation related 
    integer, parameter :: numberOfParallel = 8  ! number of parallel subconfiguration spaces for Metropolis
    integer :: iseed 

    !!!!!!!END OF: WHEN CHANGE ATOM, CHANGE THESES!!!!!!!!!!!

    

    !!!!!!!! USER DEFINED FROM COMMAND-LINE !!!!!!!!!!!!!
    ! expressions for probability and local energy, user defined 
    character(len=characterlengthEnergy) :: localenergyexpr
    character(len =characterlengthProb) :: probabilityexpr  
                 

    !!!!!!!! END OF: USER DEFINED FROM COMMAND-LINE !!!!!!!!!!!!!
 


    !!!!!!!! Command line stuff !!!!!!!!!!!!

    ! dealing with command line stuff 
    integer :: i, iarg                     ! number of command line arguments                       
    character(len=80) :: arg                    ! command line argument 
    integer :: ios 
    character(len= 3000) :: line
    !!!!!!!! END OF: Command line stuff !!!!!!!!!!!!    


    !!!!!!!! TEMPORARY VARIABLES  !!!!!!!!!!!!
    real(rn) :: localenergyres
   
    real(rn), dimension(3*numberOfParticles+numberOfParams) :: flattened_configurationAndParams 

    real(rn) :: mean,   variance, stderrorofmean, sizeofenergyarray, meanOfSquared 




    !!!!!!!!END OF:  TEMPORARY VARIABLES !!!!!!!!!!!!


    !!!!!!!! PARALLEL COMPUTATION VARIABLES !!!!!!!!!!!! 
    real(rn), allocatable :: energyInConfigSpaceList(:, :) 
    real(rn), allocatable :: localenergyInConfigSubspaceList(:)        
    real(rn) :: samplesInConfigSubspace(numberOfConfig, 3, numberOfParticles)
    integer :: k, l, m 
    character(len= 1024) ::filename 

    

    !!!!!!!! END OF: PARALLEL COMPUTATION VARIABLES !!!!!!!!!!!! 


    








    call omp_set_nested(.true.)     ! enable nested parallelism 

    call cpu_time(total_start_time)


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






    !!!!!! USER (COMMAND-LINE) INTERFACE !!!!!!!!


    !  1. filepath of probability expression 
    !  2. filepath of local energy expression


    ! check: correct number of command-line arguments 
    iarg = command_argument_count()
    if (iarg/=2) then 
        print*, "ERROR: invalid number of arguments"
        print*, "Usage: ./a.out ", & 
        "[filepath of probability expression] [filepath of local energy expr]"
        stop 
    end if 
    
        
    !1. argument: filepath probability expr
    call get_command_argument(1, arg )
    open(unit=1, file = arg, iostat= ios, status = 'old')
    if (ios/=0) then 
        print '(a, a, 5x, i0)', 'ERROR: failed in opening file named ', trim(arg), ios 
        stop 
    end if 


    ! assuming one-line expression file
    read(1, '(a)', iostat = ios) probabilityexpr 
    if (ios/=0) then 
        print '(a)', 'ERROR: reading probabilityexpr failed!'
        stop 
    end if 

    close(1)
    

    ! 2. argument: local energy file 
    call get_command_argument(2, arg )
    open(unit=1, file = arg, iostat= ios, status = 'old')
    if (ios/=0) then 
        print '(a, a, 5x, i0)', 'ERROR: failed in opening file named ', trim(arg), ios 
        stop 
    end if 



    ! assuming one-line expression file
    read(1, '(a)', iostat = ios) localenergyexpr
    if (ios/=0) then 
        print '(a)', 'ERROR: reading localenergyexpr failed! '
        stop 
    end if 

    close(1)


  
    !!!!!! END OF USER (COMMAND-LINE) INTERFACE !!!!!!!!


    ! initiate random number for Mersenne Twister 
    iseed = time()
    call sgrnd(iseed)


    ! set number of threads to be used 
    call omp_set_num_threads(numberOfParallel)

    

    allocate(energyInConfigSpaceList(numberOfConfig, numberOfParallel))                 ! save all local energies into here 
    allocate(localenergyInConfigSubspaceList(numberOfConfig))

    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!PARALLEL COMPUTATION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    

    !$OMP PARALLEL SHARED(energyInConfigSpaceList,i , parameterValues) PRIVATE(filename, localenergyInConfigSubspaceList, samplesInConfigSubspace, k, l,m,  iseed, start_time, end_time, localenergyres , flattened_configurationAndParams  ) 
    


        i =  1 ! initialize indexing for energyInConfigSpaceList

        


        !!!! POSSIBLE SEED COMES HERE: Mersenne twister initiation 
        iseed = time() + omp_get_thread_num()
        call sgrnd(iseed)


        call cpu_time(start_time)

       

        !Thermalisation 
        print*, '----------Thermalisation started------------------'
        call thermalisation(numberOfIterations, samplesInConfigSubspace) 
        print*, '----------Thermalisation ended--------------------'

        call cpu_time(end_time)
        print*, 'TIME USED IN THERMALISATION BY THREAD ', omp_get_thread_num(),':',  end_time-start_time

        

        ! write(filename, "(A22, I0)")  "thermalisation_samples", omp_get_thread_num()+4



        ! ! save thermalisation samples 
        ! open(unit=omp_get_thread_num(),file=filename,iostat=ios,status='new')
        ! ! prepared for an error
        ! if (ios/=0) then
        !     print '(a,a)','*** Error in opening file ', filename
        !     stop
        ! end if
    
        ! do m =1, numberOfConfig 
        !     write(omp_get_thread_num(), *) samplesInConfigSubspace(m, :, :)
        ! end do 

        ! close(omp_get_thread_num())





     

        call cpu_time(start_time)



        l = 1 
        ! local energy computation
        Configurationdo: DO k=1, numberOfConfig 
            
                ! make suggestion on one particle in turn: e.g. for this configuration, suggestion for 1st, next configuration suggestion for the 2nd; then 
                ! restart again from the 1st, when went through all particles 
                call stepsuggestion(samplesInConfigSubspace(k, :, :), l)    

                
                ! mechanism for deploying the previous idea
                if (l<numberOfParticles) then 
                    l =l +1          
                else 
                    l = 1 
                end if 


                !!!!!!!!!!! COMPUTING LOCAL ENERGY !!!!!!!!!!!!!!!!!

               

                ! get flattened configuration with parameters at the tail 
                flattened_configurationAndParams(: numberOfCoordinates)  = reshape(samplesInConfigSubspace(k, :, :), [numberOfCoordinates])
                flattened_configurationAndParams(numberOfCoordinates+1:) = parameterValues



                ! compute the local energy 
                call evaluateexpressions(trim(localenergyexpr), variables, flattened_configurationAndParams, localenergyres )
                
                !!!!!!!!!!! END OF: COMPUTING LOCAL ENERGY !!!!!!!!!!!!!!!!!

                ! save local energy to list 
                localenergyInConfigSubspaceList(k) = localenergyres
                

                
                

            ! printing about the progress
            if (k == numberOfConfig/2) then 
                print*, 'WE ARE ABOUT HALF WAY'

            end if 
        END DO Configurationdo



        call cpu_time(end_time)
        print*, 'TIME USED IN METROPOLIS SUGGESTION FOR ENERGY COMPUTATIONS BY THREAD ', omp_get_thread_num(),':',  end_time-start_time

    


        call cpu_time(start_time)

        
        ! compute mean local energy (i.e. MC integral energy) and save into an array 
        !$OMP CRITICAL
            
            energyInConfigSpaceList(:, i ) = localenergyInConfigSubspaceList           ! each column is separate, list of local energies, from each parallel subconfigurations 

           
            i = i + 1 
        

        !$OMP END CRITICAL

        call cpu_time(end_time)
        print*, 'TIME USED IN MEAN CALCULATION IN FOR energyInConfigSpaceList BY THREAD ', omp_get_thread_num(),':', end_time-start_time

        


    
    !$OMP END PARALLEL 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!END OF: PARALLEL COMPUTATION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    


    !!!!!!!!!!!!!!!!!!!!!!!!!! STATISTICS !!!!!!!!!!!!!!!!!!!!!!!!!!!
            


    sizeofenergyarray = size(energyInConfigSpaceList)

    ! print*, energyInConfigSpaceList
    mean = SUM(energyInConfigSpaceList)/sizeofenergyarray

    meanOfSquared = SUM(energyInConfigSpaceList **2.0)/sizeofenergyarray




    ! sample variance 
    variance = meanOfSquared - mean**2        ! E[X^2] - E[X]^2
   
    
    ! std error of mean 
    stderrorofmean = sqrt(variance/ sizeofenergyarray )

    print '(a, g0,x, g0,x, g0)', 'Mean, Variance, Std error of mean : ', mean, variance, stderrorofmean

    
    !!!!!!!!!!!!!!!!!!!!!!!!!!END OF: STATISTICS !!!!!!!!!!!!!!!!!!!!!!!!!!!
    



    call cpu_time(total_end_time)
    print*, 'TOTAL TIME USED: ', total_end_time-total_start_time
    




    contains 


        subroutine mcenergy(configurationspace, params, energy)
            implicit none
            real(rn), dimension(numberOfConfig, 3, numberOfParticles), intent(in):: configurationspace 
            real(rn), dimension(numberOfParams), intent(in) :: params
            real(rn), intent(out) :: energy 
            


            integer :: a 
            real(rn), dimension(numberOfCoordinates+numberOfParams) :: flattened_configurationAndParams 
            real(rn) :: localEnergy 
            real(rn), dimension(numberOfConfig) :: localEnergyList 


            !$OMP DO PRIVATE(a,  flattened_configurationAndParams, localEnergy)
            configurationdo: DO a=1, numberOfConfig 
                

                ! get flattened configuration with parameters at the tail 
                flattened_configurationAndParams(: numberOfCoordinates)  = reshape(configurationspace(a, :, :), [numberOfCoordinates])
                flattened_configurationAndParams(numberOfCoordinates+1:) = params 
                
        
                
                call evaluateexpressions(trim(localenergyexpr), variables, flattened_configurationAndParams, localEnergy )
    


                localEnergyList(a) = localEnergy

            END DO configurationdo
            !$OMP END DO

            
            energy = (1.0_rn /size(localEnergyList)) * SUM(localEnergyList )

        




        end subroutine mcenergy



        subroutine thermalisation(iterations, samples)
            implicit none
            integer, intent(in) :: iterations 
            real(rn),  intent(out) :: samples(numberOfConfig, 3, numberOfParticles)                     ! each table represent one configuration, each row one particle's xyz-coordinates. 
                                                                                                        ! different tables represent different independent sample  


            real(rn) :: delta = 0.5
            real(rn), allocatable :: deltai(:)
            real(rn), allocatable :: trial_configuration(:, :), current_configuration(:, : )
            real(rn) :: trial_coordinates(3)
            real(rn), allocatable :: trial_configuration_vec(:), current_configuration_vec(: )
            real(rn) :: trial_probability, old_probability 

            

            integer :: a, b, c
            real(rn) :: r, w 
            real(rn), allocatable  :: samples_temp(:, :, :)    ! temporary allocatable samples 

        

            allocate(samples_temp(numberOfConfig, 3, numberOfParticles)   )


            
            
            call random_uniform_tensor(coordinateValueRange(1), coordinateValueRange(2), samples_temp, shape(samples_temp) )

            ! print*, "Before thermalisation", maxval(samples_temp), minval(samples_temp)

            ! print*, samples 
            ! stop 



            allocate(trial_configuration(3, numberOfParticles))  ! initialize trial configuration table 
            allocate(current_configuration(3, numberOfParticles)) ! initialize current configuration table 

            ! for flattening the configuration tables into vectors 
            allocate(trial_configuration_vec(size(trial_configuration)+numberOfParams))  
            allocate(current_configuration_vec(size(current_configuration)+numberOfParams))

            ! assign parameters' values to the end 
            trial_configuration_vec(3*numberOfParticles+1:) = parameterValues
            current_configuration_vec(3*numberOfParticles+1:) = parameterValues



            ! thermalisation 
            DO a=1, iterations

                DO b=1, numberOfConfig 
                    DO c=1,  numberOfParticles 
                        ! create step size 
                        call random_uniform_vector(-delta, delta , deltai, 3) 

                        

                        trial_coordinates = samples_temp(b, :, c) + deltai 

                        trial_configuration = samples_temp(b, :, :)  ! make copy of initial samples 
                        trial_configuration(:, c )  =  trial_coordinates      ! make trial move of c-th particle

                        current_configuration = samples_temp(b, :, : )  ! make copy of current configuration 

                        trial_configuration_vec(: 3*numberOfParticles) = reshape(trial_configuration, shape(trial_configuration_vec(1:3*numberOfParticles)))    ! vectorize the trial configuration 
                        current_configuration_vec(: 3*numberOfParticles)= reshape(current_configuration, shape(current_configuration_vec(1:3*numberOfParticles)))

                        
                        ! print*, "By thread", omp_get_thread_num(), 'trial:',  trial_configuration_vec
                        ! print*, "By thread", omp_get_thread_num(), 'current:',   current_configuration_vec



                        
                        
                        !$OMP PARALLEL SECTIONS 
                        
                        !$OMP SECTION 
                        call evaluateexpressions(trim(probabilityexpr), variables, trial_configuration_vec, trial_probability )
                        ! print*, "By thread", omp_get_thread_num(), 'trial_prob:',  trial_probability
                        


                        !$OMP SECTION 
                        call evaluateexpressions(trim(probabilityexpr), variables, current_configuration_vec, old_probability ) 
                        ! print*, "By thread", omp_get_thread_num(), 'old_prob:',  old_probability
                    
                         

                        !$OMP END PARALLEL SECTIONS 

                        ! print*, "AFTER SECTION, OLD: ",old_probability
                        ! print*, "AFTER SECTION, trial: ",trial_probability

                        
                        
                        w= trial_probability/old_probability 
                        

            
                       

                        ! print*, "By thread", omp_get_thread_num(), 'w:',  w

                        if (w>=1) then 
                            current_configuration = trial_configuration
                        else if (w<1) then 
                            call random_number(r) ! generate uniformly distributed random number 0<= x <1 
                            if (r<=w) then 
                                current_configuration = trial_configuration 
                            end if 
                        end if 

                        samples_temp(b, :, :) = current_configuration

                 


                    END DO 

                END DO 

            END DO 

            



            samples = samples_temp  






        end subroutine thermalisation










        subroutine stepsuggestion(configuration, nthParticle)
            implicit none
            real(rn), parameter :: delta =0.5 

            real(rn), dimension(3, numberOfParticles), intent(inout) :: configuration 
            integer, intent(in) :: nthParticle 


            real(rn), dimension(3) :: particleOfInterest, coordinates_trial
            real(rn), allocatable :: deltai(:) 
            real(rn), dimension(3, numberOfParticles ) :: trialConfiguration
            real(rn), allocatable :: trial_configuration_vec(:), current_configuration_vec(: )
            real(rn) :: trial_probability, old_probability, w ,r 
            

            
            particleOfInterest = configuration(:, nthParticle) 
            
            ! generating step size
            call random_uniform_vector(-delta, delta , deltai, 3)  

            ! trial position 
            coordinates_trial = particleOfInterest + deltai 

            ! moving suggestion to the n-th particle
            trialConfiguration = configuration  
            trialConfiguration(:, nthParticle) = coordinates_trial 



            ! for flattening the configuration tables into vectors 
            allocate(trial_configuration_vec(size(trialConfiguration)+numberOfParams))  
            allocate(current_configuration_vec(size(configuration)+numberOfParams))

            ! assign parameters' values to the end 
            trial_configuration_vec(3*numberOfParticles+1:) = parameterValues
            current_configuration_vec(3*numberOfParticles+1:) = parameterValues


            ! metropolis suggestion 
            
            !$OMP PARALLEL SECTIONS 

            !$OMP SECTION 
            call evaluateexpressions(trim(probabilityexpr), variables, trial_configuration_vec, trial_probability )
            !$OMP SECTION 
            call evaluateexpressions(trim(probabilityexpr), variables, current_configuration_vec, old_probability ) 

            !$OMP END PARALLEL SECTIONS 
            
            
            w= trial_probability/old_probability 

            if (w>=1) then 
                configuration = trialConfiguration
            else if (w<1) then 
                call random_number(r) ! generate uniformly distributed random number 0<= x <1 
                if (r<=w) then 
                    configuration = trialConfiguration 
                end if 
            end if 






        end  subroutine stepsuggestion


        subroutine evaluateexpressions(func, var, val, res ) 
            implicit none 
      
            type(EquationParser) :: eqParser
            character(len=*),   intent(in) :: func
            INTEGER,                             PARAMETER :: nfunc = 1
      
            INTEGER,                             PARAMETER :: nvar = numberOfCoordinates+ numberOfParams
            CHARACTER (LEN=*), DIMENSION(nvar),  intent(in):: var  
            REAL(rn),          DIMENSION(nvar),  intent(in) :: val  
            REAL(rn), intent(out)                                      :: res
      
            eqParser = EquationParser(func, var)
      
            res = eqParser%evaluate(val)
         
      
            
        end subroutine evaluateexpressions







        !!!!!!!!!!!!!!RANDOM  NUMBER GENERATORS !!!!!!!!!!!!!!!!!!!!


        
        ! subroutine random_uniform_tensor_forThermalisation(lower, upper, arr)
        !     implicit none 
        !     real(rn),intent(in) :: lower, upper     ! lower and upper limit 
        !     real(rn), allocatable, intent(out) :: arr(numberOfConfig, 3, numberOfParticles) 

        !     integer :: a, b, c 
        !     real(rn) :: random_number 




        !     do a= 1, arr_shape(1)
        !         do b =1, arr_shape(2)
        !             do c=1, arr_shape(3)
        !                 call random_uniform(lower, upper, random_number)
        !                 arr(a, b, c  ) = random_number
        !             end do 
        !         end do 
        !     end do 
            



        ! end subroutine random_uniform_tensor


        subroutine random_uniform_tensor(lower, upper, arr, arr_shape )
            implicit none 
            real(rn),intent(in) :: lower, upper     ! lower and upper limit 
            integer, dimension(3), intent(in) :: arr_shape 
            real(rn), allocatable, intent(out) :: arr(:, :, : )

            integer :: a, b, c 
            real(rn) :: random_number 


            allocate(arr(arr_shape(1), arr_shape(2), arr_shape(3)))


            do a= 1, arr_shape(1)
                do b =1, arr_shape(2)
                    do c=1, arr_shape(3)
                        call random_uniform(lower, upper, random_number)
                        arr(a, b, c  ) = random_number
                    end do 
                end do 
            end do 
            



        end subroutine random_uniform_tensor




        subroutine random_uniform_array(lower, upper, arr, arr_shape )
            implicit none 
            real(rn),intent(in) :: lower, upper     ! lower and upper limit 
            integer, dimension(2), intent(in) :: arr_shape 
            real(rn), allocatable, intent(out) :: arr(:, :)

            integer :: a, b 
            real(rn) :: random_number 


            allocate(arr(arr_shape(1), arr_shape(2)))


            do a= 1, arr_shape(1)
                do b =1, arr_shape(2)
                        call random_uniform(lower, upper, random_number)
                        arr(a, b ) = random_number
                end do 
            end do 
            



        end subroutine random_uniform_array

        subroutine random_uniform_vector(lower, upper, vec, vec_length )
            implicit none 
            real(rn),intent(in) :: lower, upper     ! lower and upper limit 
            integer, intent(in) :: vec_length
            real(rn), allocatable, intent(out) :: vec(:)

            integer :: a, b 
            real(rn) :: random_number 


            allocate(vec(vec_length))


            do a= 1, vec_length
                call random_uniform(lower, upper, random_number)
                vec(a) = random_number
            end do 
            



        end subroutine random_uniform_vector




        ! assuming a<b, generate uniform distribution a<x<=b 
        subroutine random_uniform(a,b,x)
            implicit none
            real(rn),intent(in) :: a,b
            real(rn),intent(out) :: x
            ! integer, optional, intent(inout) :: iseed 
            real(rn) :: u


            ! if (present(iseed).eqv..false.) then 
            !     iseed = time()
            ! end if 

            call random_stduniform(u )
            x = (b-a)*u + a
        end subroutine random_uniform



        subroutine random_stduniform(u)
            implicit none
            real(rn),intent(out) :: u
            ! integer, optional, intent(inout) :: iseed 
            real(rn) :: r
            

            ! if (present(iseed).eqv..false.) then 
            !     iseed = time()
            ! end if 


            call random_number(r)

            u = 1 - r
         end subroutine random_stduniform

        subroutine random_number(r )
            implicit none
            real(rn), intent(out) :: r 
            ! integer, optional, intent(inout) :: iseed 

            ! if (present(iseed).eqv..false.) then 
            !     iseed = time()
            ! end if 
        
            ! call sgrnd(iseed)

            r = grnd()

        end subroutine random_number




        !!!!!!!!!!!!!!END OF: RANDOM  NUMBER GENERATORS !!!!!!!!!!!!!!!!!!!!





        







    
end program main