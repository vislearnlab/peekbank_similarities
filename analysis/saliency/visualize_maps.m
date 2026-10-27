%VISUALIZE_MAPS  GBVS target/distractor saliency for every Peekbank image pair.
%   For each pair {A, B} in peekbank_stimuli.csv this renders BOTH composites
%   (A left / B right, and B left / A right). GBVS normalises over the whole
%   composite, so saliency depends on which side each image is on, and a sign
%   flip of the other layout is only an approximation.
%
%   Each composite yields two output rows, one per role assignment:
%       trial_key = "<target_id>_<distractor_id>", plus target_side
%   so the output joins straight onto trials on (trial_key, target_side).
%
%   For every composite two images are saved:
%       data/composites/<left>_<right>.png            raw composite
%       data/composites_saliency/<left>_<right>.png   GBVS heatmap overlay only
%   Nothing is displayed on screen.

%% Paths
script_dir = fileparts(mfilename('fullpath'));
if isempty(script_dir); script_dir = pwd; end   % e.g. when run as a selection
D = loadenv(fullfile(script_dir, '..', '..', '.env'));
PROJECT_PATH = D("PROJECT_PATH");
fprintf('PROJECT_PATH = %s\n', PROJECT_PATH);

%% Config
ID1_COL       = "target_id";   % id columns in peekbank_stimuli.csv; if missing,
ID2_COL       = "distractor_id";   % the image file name (no extension) is used instead
save_every    = 1;            % checkpoint the CSV every Nth pair

comp_dir    = fullfile(PROJECT_PATH, 'data', 'composites');
sal_dir     = fullfile(PROJECT_PATH, 'data', 'composites_saliency');
output_file = fullfile(PROJECT_PATH, 'data', 'metadata', 'level-trial_added-saliency_data.csv');
if ~exist(comp_dir, 'dir'); mkdir(comp_dir); end
if ~exist(sal_dir,  'dir'); mkdir(sal_dir);  end

%% Load pairs
image_pairs = readtable(fullfile(PROJECT_PATH, 'data', 'peekbank_stimuli.csv'), 'TextType', 'string');
n_pairs = height(image_pairs);

has_ids = all(ismember([ID1_COL, ID2_COL], string(image_pairs.Properties.VariableNames)));
if ~has_ids
    warning('Columns %s / %s not found; using image file names as ids.', ID1_COL, ID2_COL);
end

%% Main loop
rows = cell(4 * n_pairs, 1);
row = 0;

for i = 1:n_pairs
    path1 = string(image_pairs.image1(i));
    path2 = string(image_pairs.image2(i));
    if has_ids
        id1 = string(image_pairs.(ID1_COL)(i));
        id2 = string(image_pairs.(ID2_COL)(i));
    else
        [~, id1] = fileparts(path1);
        [~, id2] = fileparts(path2);
    end
    im1 = imread(fullfile(PROJECT_PATH, 'data', path1));
    im2 = imread(fullfile(PROJECT_PATH, 'data', path2));

    layouts = {id1, im1, id2, im2;    % image1 on the left
               id2, im2, id1, im1};   % image2 on the left
    for k = 1:2
        [left_id, left_im, right_id, right_im] = layouts{k, :};

        % Raw composite
        [canvas, left_rect, right_rect] = make_pair_canvas(left_im, right_im);
        comp_name = left_id + "_" + right_id;
        imwrite(canvas, fullfile(comp_dir, comp_name + ".png"));

        % Saliency overlay (heatmap only, no boxes or text)
        [overlay, s] = compare_saliency(canvas, left_rect, right_rect);
        imwrite(overlay, fullfile(sal_dir, comp_name + ".png"));

        L = [s.MeanTargetSaliency,     s.MaxTargetSaliency,     s.SDTargetSaliency];
        R = [s.MeanDistractorSaliency, s.MaxDistractorSaliency, s.SDDistractorSaliency];

        row = row + 1; rows{row} = make_row(left_id,  right_id, "left",  comp_name, L, R);
        row = row + 1; rows{row} = make_row(right_id, left_id,  "right", comp_name, R, L);
    end

    if mod(i, save_every) == 0
        writetable(vertcat(rows{1:row}), output_file);
        fprintf('Saved progress after %d/%d image pairs\n', i, n_pairs);
    end
end

%% Final save (drop duplicates if the CSV lists a pair in both orders)
results = vertcat(rows{1:row});
[~, ia] = unique(results(:, {'trial_key', 'target_side'}), 'rows', 'stable');
results = results(ia, :);
writetable(results, output_file);
fprintf('Wrote %d rows to %s\n', height(results), output_file);


%% ===================== Local functions =====================

function t = make_row(target_id, distractor_id, target_side, composite, T, D)
%MAKE_ROW  One output row. T and D are [mean max sd] for target and distractor.
    v = round([T, D, T(1) - D(1)], 4);
    t = table(target_id + "_" + distractor_id, target_id, distractor_id, ...
              target_side, composite, v(1), v(2), v(3), v(4), v(5), v(6), v(7), ...
        'VariableNames', {'trial_key', 'target_id', 'distractor_id', 'target_side', 'composite', ...
                          'MeanTargetSaliency', 'MaxTargetSaliency', 'SDTargetSaliency', ...
                          'MeanDistractorSaliency', 'MaxDistractorSaliency', 'SDDistractorSaliency', ...
                          'MeanSaliencyDiff'});
end


function [overlay, saliency_data] = compare_saliency(img, left_rect, right_rect, saliency_map)
%COMPARE_SALIENCY  Left/right object saliency for a two-image composite.
%   LEFT_RECT / RIGHT_RECT are the [x y w h] placement rectangles from
%   make_pair_canvas. "Target" in the returned table = LEFT, "Distractor" = RIGHT.
%   OVERLAY is the GBVS heatmap blended over the composite at full canvas
%   resolution, with no boxes, labels or title.
    if nargin < 4 || isempty(saliency_map)
        out_gbvs = gbvs(img);
        saliency_map = out_gbvs.master_map_resized;
    else
        saliency_map = imresize(saliency_map, [size(img, 1), size(img, 2)]);
    end

    left_bbox  = object_bbox(img, left_rect);
    right_bbox = object_bbox(img, right_rect);

    [mean_l, max_l, sd_l] = extract_saliency(saliency_map, left_bbox);
    [mean_r, max_r, sd_r] = extract_saliency(saliency_map, right_bbox);

    saliency_data = table(mean_l, max_l, sd_l, mean_r, max_r, sd_r, ...
        'VariableNames', {'MeanTargetSaliency', 'MaxTargetSaliency', 'SDTargetSaliency', ...
                          'MeanDistractorSaliency', 'MaxDistractorSaliency', 'SDDistractorSaliency'});

    overlay = im2uint8(mat2gray(heatmap_overlay(img, saliency_map)));
end


function bbox = object_bbox(img, cell_rect, tol)
%OBJECT_BBOX  Tight [x y w h] box around the object inside a placement rectangle.
%   Background colour is sampled from the rectangle's corners, so the input
%   must be the full placement rect, not an already-tight box.
    if nargin < 3 || isempty(tol)
        tol = 25;   % colour distance 0-255. Raise if bg leaks in, lower if object clips.
    end

    x = round(cell_rect(1)); y = round(cell_rect(2));
    w = round(cell_rect(3)); h = round(cell_rect(4));

    [H, W, ~] = size(img);
    x = max(1, x); y = max(1, y);
    w = min(w, W - x + 1); h = min(h, H - y + 1);

    sub = double(img(y:y+h-1, x:x+w-1, :));

    k = max(3, round(0.05 * min(h, w)));
    corners = [reshape(sub(1:k,         1:k,         :), [], 3);
               reshape(sub(1:k,         end-k+1:end, :), [], 3);
               reshape(sub(end-k+1:end, 1:k,         :), [], 3);
               reshape(sub(end-k+1:end, end-k+1:end, :), [], 3)];
    bg = median(corners, 1);

    dist = sqrt(sum((sub - reshape(bg, 1, 1, 3)).^2, 3));
    fg = dist > tol;

    fg = bwareaopen(fg, round(5e-4 * w * h));
    fg = imclose(fg, strel('disk', 7));
    fg = imfill(fg, 'holes');

    [rr, cc] = find(fg);
    if isempty(rr)
        bbox = [x, y, w, h];   % fallback: whole rectangle
        return
    end
    bbox = [x + min(cc) - 1, y + min(rr) - 1, ...
            max(cc) - min(cc) + 1, max(rr) - min(rr) + 1];
end


function [mean_s, max_s, sd_s] = extract_saliency(saliency_map, bbox)
%EXTRACT_SALIENCY  Mean/max/SD of the saliency map inside an [x y w h] box.
    [H, W] = size(saliency_map);
    x1 = max(1, round(bbox(1)));
    y1 = max(1, round(bbox(2)));
    x2 = min(W, round(bbox(1) + bbox(3) - 1));
    y2 = min(H, round(bbox(2) + bbox(4) - 1));

    region = saliency_map(y1:y2, x1:x2);
    mean_s = mean(region(:));
    max_s  = max(region(:));
    sd_s   = std(region(:));
end


function cell = fit_in_cell(im, cellH, cellW, grayVal)
%FIT_IN_CELL  Scale an image to fit a cell (aspect preserved), centred on gray.
    if ismatrix(im); im = repmat(im, [1 1 3]); else; im = im(:, :, 1:3); end
    if ~isa(im, 'uint8'); im = im2uint8(im); end

    [h, w, ~] = size(im);
    s = min(cellW / w, cellH / h);
    im = imresize(im, s);

    [h, w, ~] = size(im);
    cell = uint8(grayVal * ones(cellH, cellW, 3));
    r0 = floor((cellH - h) / 2);
    c0 = floor((cellW - w) / 2);
    cell(r0 + (1:h), c0 + (1:w), :) = im;
end


function [canvas, pos1, pos2] = make_pair_canvas(img1, img2, opts)
%MAKE_PAIR_CANVAS  Place two images left/right on a 16:9 gray canvas.
    arguments
        img1
        img2
        opts.CanvasW (1,1) double = 1920
        opts.CanvasH (1,1) double = 1080
        opts.CellW   (1,1) double = 600
        opts.CellH   (1,1) double = 600
        opts.Margin  (1,1) double = 120
        opts.Gray    (1,1) double = 128
    end

    canvas = uint8(opts.Gray * ones(opts.CanvasH, opts.CanvasW, 3));

    img1 = fit_in_cell(img1, opts.CellH, opts.CellW, opts.Gray);
    img2 = fit_in_cell(img2, opts.CellH, opts.CellW, opts.Gray);

    rowTop = round((opts.CanvasH - opts.CellH) / 2);
    r  = rowTop + (1:opts.CellH);
    c1 = opts.Margin + (1:opts.CellW);
    c2 = opts.CanvasW - opts.Margin - opts.CellW + (1:opts.CellW);

    canvas(r, c1, :) = img1;
    canvas(r, c2, :) = img2;

    pos1 = [c1(1) r(1) opts.CellW opts.CellH];   % [x y w h]
    pos2 = [c2(1) r(1) opts.CellW opts.CellH];
end