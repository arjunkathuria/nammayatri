ALTER TABLE atlas_app.quote ADD COLUMN merchant_id character varying(36);

ALTER TABLE atlas_app.quote ALTER COLUMN merchant_id SET NOT NULL;