ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN provider_id character(36);

ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN provider_id SET NOT NULL;