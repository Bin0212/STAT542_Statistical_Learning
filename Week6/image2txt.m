img = imread('mountain.jpeg');
img = rgb2gray( img );
fid = fopen('YourImage.txt', 'w');
if fid == -1, error('Cannot open file'); end
fprintf(fid, '%d ', size(img));  % Assuming it is an RGB image
fprintf(fid, '%g ', img(:));
fclose(fid);