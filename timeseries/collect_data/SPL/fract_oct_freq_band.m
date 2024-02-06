function [fexact_l_c_u, fnominal_l_c_u, fnominal_str_l_c_u] = fract_oct_freq_band(N, min_f, max_f, SI_prefixes, base10, fr)
% % fract_oct_freq_band: Calculates the 1/nth octave band center, lower, and upper bandedge frequency limits
% % 
% % Syntax;    
% % 
% % [fexact_l_c_u, fnominal_l_c_u, fnominal_str_l_c_u] = fract_oct_freq_band(N, min_f, max_f, SI_prefixes, base10, fr);
% % 
% % **********************************************************************
% % 
% % Description
% % 
% % This program calculates the fractional, 1/N octave band center 
% % frequencies and the lower and upper bandedge limits of every 
% % frequency band from min_f to max_f (Hz).
% % 
% % The exact and nominal values of the frequencies are output. 
% % The exact values are useful for calculations and making filters.  
% % The nominal values are useful for tables, plots, figures, and graphs.
% % 
% % The frequency band nominal values are rounded to three or more 
% % significant digits based on the number of bands per octave.  
% % Full octave and third octave bands are rounded to three significant digits. 
% % 
% % The exact and nominal frequency values and character strings are output.  
% % 
% % fr default 1000 Hz
% % fr Hz is a reference frequency when N is odd, odd-fractional-octave-bands.  
% % fr Hz is an edge frequency when N is even, even-fractional-octave-bands.  
% % 
% % fract_oct_freq_band produces exact and nominal frequency bands which 
% % satisfy ANSI S1.6-R2006 and ANSI S1.11-R2014.    
% % 
% % 
% % fract_oct_freq_band is a modification of centr_freq
% % centr_freq can be found on Matlab Central File Exchange
% % The Matlab ID is 17590.
% % 
% % **********************************************************************
% % 
% % Input Variables
% % 
% % N=3;            % one-third-octave-bands which have three bands per octave.
% %                 % 1 one band per octave.  
% %                 % N is the number of frequency bands per octave.  
% %                 % N can be any integrer > 0.  
% %                 % Default is 3 for one-third-octave-bands.  
% % 
% % min_f=20;       % min_f is the minimum frequency band to calculate (Hz).
% %                 % min_f > 0. Must be graeater than 0.  
% %                 % default is 20;
% % 
% % max_f=20000;    % max_f is the maximum frequency band to calculate (Hz).
% %                 % max_f > 0. Must be graeater than 0.  
% %                 % default is 20000;
% % 
% % SI_prefixes=0;  % This parameter controls the output for the character 
% %                 % strings of the nominal frequencies.
% %                 % 1 for using SI prefixes (i.e. k for 1000)
% %                 % 0 for not using prefixes.
% %                 % default is SI_prefixes=0;
% % 
% % base10=1;       % 1 for using base 10 filter frequencies
% %                 % 0 for using base 2 filter frequencies
% %                 % otherwise  base 10 filter frequencies
% %                 % default base 10 filter frequencies
% % 
% % fr=1000;        % fr is the preferred central frequency for 
% %                 % calculating all of the other center frequencies.   
% %                 % default is 1000;
% % 
% % **********************************************************************
% % 
% % Output Variables
% % 
% % fexact_l_c_u is a 2D array of exact frequency bands in Hz.
% % fexact_l_c_u( frequecy bands, [lower=1, center=2, upper =3;] );
% % 
% % fnominal_l_c_u is a 2D array of nominal frequency bands in Hz.
% % fnominal_l_c_u{ frequecy bands, [lower=1, center=2, upper =3;] };
% % 
% % fnominal_str_l_c_u is a 2D cell array of nominal frequency band strings in Hz.
% % fnominal_str_l_c_ufnominal_l_c_u{ frequecy bands, [lower=1, center=2, upper =3;] };
% % 
% % **********************************************************************
% 
% 
% Example='1';
% 
% % Full-octave-band center frequencies from 20 Hz to 20000 Hz
% close('all');
% [fexact_l_c_u, fnominal_l_c_u, fnominal_str_l_c_u] = fract_oct_freq_band(1, 20, 20000);
% figure(1);
% semilogy(fexact_l_c_u(:, 1), ':ro', 'linewidth', 4, 'markersize', 10);
% hold on;
% semilogy(fexact_l_c_u(:, 2), '-gp', 'linewidth', 4, 'markersize', 10);
% semilogy(fexact_l_c_u(:, 3), '-.cs', 'linewidth', 4, 'markersize', 10);
% semilogy(fnominal_l_c_u(:, 1), '-.kx', 'linewidth', 3, 'markersize', 14);
% semilogy(fexact_l_c_u(:, 2), ':m*', 'linewidth', 3, 'markersize', 14);
% semilogy(fexact_l_c_u(:, 3), '--b+', 'linewidth', 3, 'markersize', 10);
% legend({'Exact lower band', 'Exact center', 'Exact upper band', ...
%         'Nominal lower band','Nominalcenter', 'Nominal upper band'}, ...
%         'location', 'northwest');
% set(gca, 'fontsize', 24);
% maximize(gcf);
% 
% 
% % one-third-octave-band center frequencies from 20 Hz to 20000 Hz
% [fexact_l_c_u, fnominal_l_c_u, fnominal_str_l_c_u] = fract_oct_freq_band(3, 20, 20000);
% 
% % one-twelveth-octave-band center frequencies from 100 Hz to 10000 Hz
% [fexact_l_c_u, fnominal_l_c_u, fnominal_str_l_c_u] = fract_oct_freq_band(12, 100, 10000);
% 
% % twenty-fourth-octave-band center frequencies from 0.001 Hz to 10000000 Hz
% [fexact_l_c_u, fnominal_l_c_u, fnominal_str_l_c_u] = fract_oct_freq_band(24, 0.001, 10000000);
% 
% 
% % **********************************************************************
% % 
% % References
% % 
% % 1)  ANSI S1.6-R2006 Preferred Frequencies, Frequency Levels, and 
% %     Band Numbers for Acoustical Measurements, 1984.
% % 
% % 2)  ANSI S1.11-2014, (2014). American National Standard Electroacoustics
% %     - Octave-band and Fractional-octave-band Filters - Part 1: 
% %     Specifications (a nationally adopted international standard). 
% %     American National Standards Institute, Acoustical Society of 
% %     America, New York 
% % 
% % 
% % **********************************************************************
% % 
% % fract_oct_freq_band is a modification of centr_freq
% % centr_freq can be found on Matlab Central File Exchange
% % The Matlab ID is 17590.
% % 
% % Written by   Author  Eleftheria  
% %              E-mail  elegeor@gmail.com 
% %              Company/University: University of Patras 
% % 
% % 
% % **********************************************************************
% % 
% % List of Dependent Subprograms for 
% % fract_oct_freq_band
% % 
% % FEX ID# is the File ID on the Matlab Central File Exchange
% % 
% % 
% % Program Name   Author   FEX ID#
% % 1) sd_round		Edward L. Zechmann			
% % 
% % 
% % **********************************************************************
% % 
% % 
% % Program Modified by Edward L. Zechmann
% % 
% % modified  3 March       2008    Original modification of program
% %                                 updated comments 
% % 
% % modified 13 August      2008    Updated comments.  
% %   
% % modified 18 August      2008    Added rounding last digit to nearest 
% %                                 multiple of 5.  Added Examples.  
% %                                 Updated comments.  
% %    
% % modified 21 August      2008    Fixed a bug in usign min_f and max_f 
% %                                 which does not include 1000 Hz.  
% %                                 Zipped the depended program sd_round. 
% %                                 Updated comments.  
% % 
% % modified 18 November    2008   	Added additional rounding 
% % 
% % modified  8 December    2008    Updated comments.
% % 
% % modified 18 December    2008    Updated comments.
% % 
% % modified  4 January     2008    Changed rounding of the lower and upper 
% %                                 bandedge frequency limits.
% % 
% % modified  6 October     2009    Updated comments
% % 
% % modified 22 January     2010    Modified the number of significant 
% %                                 digits for rounding. The number of  
% %                                 significnat digits increases as the
% %                                 number of bands per octave increases.  
% %                                 This supports high resolution octave
% %                                 band analysis.
% % 
% % modified  4 October     2014    Modified the even-octave-bands to have
% %                                 1000 Hz as an edge frequency.  
% %                                 Changed the number of significant
% %                                 digits calculation for rounding. 
% %                                 Limited the number of bands per octave 
% %                                 from 1 to 43.
% % 
% % modified 21 February    2019    Revised the number of significant
% %                                 digits calculation for rounding. 
% %                                 Limited the number of bands per octave 
% %                                 from 1 to 10E12.
% %                                 
% %                                 A warning is issued if the number of
% %                                 bands is greater than 10E6.
% %  
% % modified 22 February    2019    Added the reference frequency, fr as an 
% %                                 input variable and optimized the 
% %                                 removal of fractional-octave-bands
% %                                 outside the range of min_f to max_f. 
% %                                  
% % 
% % **********************************************************************
% % 
% % Please Feel Free to Modify This Program
% %   
% % See Also: centr_freq, sd_round, nth_freq_band
% %   

if (nargin < 1 || isempty(N)) || ~isnumeric(N)
    N=3;
end

% N must be an integrer;
N=round(N);

% N is limited to between 1 and 10^12.
% Computer seems to lock up for N > 10^12
N(N < 1)=1;
N(N > 10^12)=10^12;

if (nargin < 2 || isempty(min_f)) || (logical(min_f < 0) || ~isnumeric(min_f))
    min_f=20;
end

if (nargin < 3 || isempty(max_f)) || (logical(max_f < 0) || ~isnumeric(max_f))
    max_f=20000;
end

if (nargin < 4 || isempty(SI_prefixes)) ||  ~isnumeric(SI_prefixes)
    SI_prefixes=0;
end

if (nargin < 5 || isempty(base10)) || ~isnumeric(base10)
    base10=1;
end

if base10 ~= 0
    base10=1;
end

if (nargin < 6 || isempty(fr)) || ~isnumeric(fr)
    fr=1000;
end

% This program uses different symbols than in the standard.  
% 
% N is used as the band designator.
% In ANSI S1.11-2014 the letter b is the band designator.  

% Determine if N is odd or even.  
% Noe stands for N-odd-even
Noe=mod(N-1, 2);
% Noe equals 0 when N is odd
% Noe equals 1 when N is even


% Now the nominal frequencies can be calculated

% Calculate the base number G  
if base10 == 1
    G=10^(3/10);
else
    G=2;
end

% Estimate the number of bands being output and return a warning if the
% array is too large. 

Nbands_total=ceil(N.*(log(max_f)-log(min_f))./log(G));

if Nbands_total > 10^6
    warning('output array estimated to be larger than 10E6 elements')
end


if fr > min_f
    % going down to get just below min_f
    % a minus sign indicates going down
    Num_bands_root_to_fmin = -ceil(N.*(log(fr)-log(min_f))./log(G));
else
    % going up to get just below min_f
    % a plus sign indicates going up
    Num_bands_root_to_fmin = +floor(N.*(log(min_f)-log(fr))./log(G));
end

% check if band is just below min_f

log_fmin_est=log(fr)-(Noe/((1+Noe)*N)).*log(G)+Num_bands_root_to_fmin./N.*log(G);

while log_fmin_est < log(min_f+eps)
    Num_bands_root_to_fmin=Num_bands_root_to_fmin+1;
    log_fmin_est=log(fr)-(Noe/((1+Noe)*N)).*log(G)+Num_bands_root_to_fmin./N.*log(G);
end

while log_fmin_est > log(min_f+eps)
    Num_bands_root_to_fmin=Num_bands_root_to_fmin-1;
    log_fmin_est=log(fr)-(Noe/((1+Noe)*N)).*log(G)+Num_bands_root_to_fmin./N.*log(G);
end

if fr > max_f
    % going down to get just above max_f
    % a minus sign indicates going down
    Num_bands_root_to_fmax = -floor(N.*(log(fr)-log(max_f))./log(G));
else
    % going up to get just above max_f
    % a plus sign indicates going up
    Num_bands_root_to_fmax = +ceil(N.*(log(max_f)-log(fr))./log(G));
end

% check if band is just above max_f

log_fmax_est=log(fr)+(Noe/((1+Noe)*N)).*log(G)+Num_bands_root_to_fmax/N.*log(G);

while log_fmax_est > log(max_f+eps)
    Num_bands_root_to_fmax=Num_bands_root_to_fmax-1;
    log_fmax_est=log(fr)+(Noe/((1+Noe)*N)).*log(G)+Num_bands_root_to_fmax/N.*log(G);
end

while log_fmax_est < log(max_f-eps)
    Num_bands_root_to_fmax=Num_bands_root_to_fmax+1;
    log_fmax_est=log(fr)+(Noe/((1+Noe)*N)).*log(G)+Num_bands_root_to_fmax/N.*log(G);
end


log_fc = log_fmin_est:(log(G)/N):log_fmax_est;

% Remove bands above the extended max_f limit
log_fc = log_fc( log_fc < log(max_f)+0.5*log(G)/N );

% Remove bands below the extended min_f limit
log_fc = log_fc( log_fc > log(min_f)-0.5/N*log(G) );

% Make center frequency bands unique
log_fc = unique(log_fc);           
log_fc = log_fc(:);


% Calculate the number of center frequency bands
num_bands=length(log_fc);

fc=exp(log_fc);

% fc is the array of exact center frequencies;
fc_exact=fc;



% ***********************************************************************
% 
% Calculate the lower and upper bounds for the lower and upper
% band edge frequencies
flu = exp([ log_fc(1)-1./(2.*N)*log(G); log_fc+(1./(2.*N)).*log(G) ]);     


% Form the numeric arrays for the exact lower frequency band edge limits.
fl_exact=flu(1:num_bands);

% Form the numeric arrays for the exact upper frequency band edge limits.
fu_exact=flu(1+(1:num_bands));

% ************************************************************************
% 
% Calculate the number of significant digits for rounding the exact
% frequencies into the nominal frequencies.
% 
% Full-octave-band and one-third-octave-band must have 3 significant digits.  
% the number of significant digits increases as log10(N).  
% 

num_sd_digits=floor(3+log10(N));
m=10.^(num_sd_digits-1);


% ************************************************************************
% 
% Calculations for nominal frequencies
% 
% Apply appropriate rounding to the center frequencies
[fc,   fc_str]   = sd_round(fc, num_sd_digits, 1, 5, SI_prefixes);
[fc2,  fc_str2]  = sd_round(fc, num_sd_digits, 1, 100, SI_prefixes);

% If the center frequency rounded to 3 significant digits and the last 
% digit rounded to the nearest multiple of 5 is within 1% of the center 
% frequency rounded to 1 significant digit, then round to 1 significant 
% digit. 
%
ix=find(abs(m*(1-exp( log(fc)-log(fc2) ))) < 1);
fc(ix)=fc2(ix);
fc_str(ix)=fc_str2(ix);

% Nominal frequencies
fc_nom=fc;
fc_nom_str=fc_str;


% Apply the same rounding technique to the lower and upper frequency 
% bandedge limits as was applied to the center frequencies.  
[flu,   flu_str]   = sd_round(flu, num_sd_digits, 1, 5, SI_prefixes);
[flu2,  flu_str2]  = sd_round(flu, num_sd_digits, 1, 100, SI_prefixes);


% If the center frequency rounded to 3 significant digits and the last 
% digit rounded to the nearest multiple of 5 is within 1% of the center 
% frequency rounded to 1 significant digit, then round to 1 significant 
% digit. 
%
ix=find(abs(m*(1-exp( log(flu)-log(flu2) ))) < 1);
flu(ix)=flu2(ix);
flu_str(ix)=flu_str2(ix);

% Form the numeric and string arrays for the nominal lower frequency 
% band edge limits.
fl_nom=flu(1:num_bands);
fl_nom_str=flu_str(1:num_bands);

% Form the numeric and string arrays for the nominal upper frequency 
% band edge limits.
fu_nom=flu(1+(1:num_bands));
fu_nom_str=flu_str(1+(1:num_bands));

% Concatenate the outputs
fexact_l_c_u=[fl_exact, fc_exact, fu_exact];
fnominal_l_c_u=[fl_nom, fc_nom, fu_nom];

fnominal_str_l_c_u=cell(num_bands, 3);
for e1=1:num_bands
    fnominal_str_l_c_u{e1, 1}=fl_nom_str{e1}; 
    fnominal_str_l_c_u{e1, 2}=fc_nom_str{e1}; 
    fnominal_str_l_c_u{e1, 3}=fu_nom_str{e1}; 
end

