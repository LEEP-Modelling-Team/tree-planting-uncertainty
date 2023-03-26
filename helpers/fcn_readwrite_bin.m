function [readmat] = fcn_readwrite_bin(mode, file, mat, dim)
    % Read and write files to binary
    % -----------------------------
    % mode: 'w' is write (overwrites existing file), 'a' is append, and 'r' is read
    % file: path of the file to read/ write
    % mat: the matrix to write
    % dim: dimensions of the matrix to read

    % Read in data from text file & write to bin
    % ------------------------------------------
    if strcmp(mode, 'w') || strcmp(mode, 'a')

        % Write matrix to the end of the file
        % -----------------------
        savename = file;
        fid = fopen(savename,mode);
        fwrite(fid, mat, 'single');
        fclose(fid);             

    else
        
        % Read matrix from file
        % -----------------------
        fid = fopen(file);
        if exist('dim', 'var')
            readmat = fread(fid, dim, 'single'); 
        else
            readmat = fread(fid, 'single');
        end
        fclose(fid);    

    end

end